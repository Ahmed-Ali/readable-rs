mod article;

use crate::logging::PerfConsoleListener;
use crate::logging::logger::*;
use crate::logging::logging_defs::*;
use crate::node_ext::NodeScoreStore;

use crate::parser::{NodeExt, NodeRef, parse_html};
use crate::utils::*;
use crate::*;
use std::rc::Rc;

fn create_perf_logger() -> PerfLogger {
    let listeners = vec![Listener::new(Rc::new(PerfConsoleListener {}))];
    PerfLogger::new(listeners)
}

mod extractor_utils {
    use crate::logging::logger::*;
    use crate::logging::logging_defs::*;
    use crate::models::Metadata;
    use crate::parser::{NodeExt, NodeRef, new_html_element};
    use crate::utils::{
        get_next_node, get_normalized_text_content, is_phrasing_content, is_whitespace_node,
        next_element, normalize_text, remove_comment_nodes, remove_tags_with_selector,
        rename_tags_with_selector, select_descendants, test_any_node_by_selector, text_similarity,
        unescape_html_entities, word_count,
    };
    use regex::Regex;
    use serde_json::Value;
    use std::collections::{HashMap, HashSet};
    use std::hash::BuildHasher;
    use std::sync::LazyLock;
    use url::Url;

    // These are the classes that readability sets itself.
    pub static CLASSES_TO_PRESERVE: LazyLock<HashSet<&'static str>> =
        LazyLock::new(|| HashSet::from(["page"]));

    pub static JSON_LD_ARTICLE_TYPES: LazyLock<Regex> = LazyLock::new(|| {
        Regex::new(r"^Article|AdvertiserContentArticle|NewsArticle|AnalysisNewsArticle|AskPublicNewsArticle|BackgroundNewsArticle|OpinionNewsArticle|ReportageNewsArticle|ReviewNewsArticle|Report|SatiricalArticle|ScholarlyArticle|MedicalScholarlyArticle|SocialMediaPosting|BlogPosting|LiveBlogPosting|DiscussionForumPosting|TechArticle|APIReference$").unwrap()
    });
    pub static JSON_LD_CONTEXT: LazyLock<Regex> =
        LazyLock::new(|| Regex::new(r"^https?://schema\.org/?$").unwrap());
    // property is a space-separated list of values
    pub static META_PROPERTY_PATTERN: LazyLock<Regex> = LazyLock::new(|| {
        Regex::new(
            r"(?i)\s*(article|dc|dcterm|og|twitter)\s*:\s*(author|creator|description|published_time|title|site_name)\s*",
        ).unwrap()
    });
    // name is a single value
    pub static META_NAME_PATTERN: LazyLock<Regex> = LazyLock::new(|| {
        Regex::new(
            r"(?i)^\s*(?:(dc|dcterm|og|twitter|parsely|weibo:(article|webpage))\s*[-\.:]\s*)?(author|creator|pub-date|description|title|site_name)\s*$",
        ).unwrap()
    });
    pub static TITLE_SEPARATOR: LazyLock<Regex> =
        LazyLock::new(|| Regex::new(r"\s[|\-–—\\/>»]\s").unwrap());
    pub static TITLE_HIERARCHICAL_SEPARATOR: LazyLock<Regex> =
        LazyLock::new(|| Regex::new(r"\s[\\/>»]\s").unwrap());
    pub static TITLE_STRIP_PREFIX: LazyLock<Regex> =
        LazyLock::new(|| Regex::new(r#"^[^\|\-–—\\/>»]*[|\-–—\\/>»]"#).unwrap());

    #[derive(Default)]
    struct JsonLdMetadata {
        title: Option<String>,
        by_line: Option<String>,
        site_name: Option<String>,
        excerpt: Option<String>,
        published_time: Option<String>,
    }

    fn json_ld_type_matches(value: &Value) -> bool {
        if let Some(typ) = value.get("@type") {
            if let Some(s) = typ.as_str() {
                return JSON_LD_ARTICLE_TYPES.is_match(s);
            }
            if let Some(arr) = typ.as_array() {
                return arr.iter().any(|v| {
                    v.as_str()
                        .map(|s| JSON_LD_ARTICLE_TYPES.is_match(s))
                        .unwrap_or(false)
                });
            }
        }
        false
    }

    pub(crate) fn json_ld_context_matches(value: &Value) -> bool {
        if let Some(ctx) = value.get("@context") {
            if let Some(s) = ctx.as_str() {
                return JSON_LD_CONTEXT.is_match(s);
            }
            if let Some(obj) = ctx.as_object() {
                if let Some(vocab) = obj.get("@vocab").and_then(|v| v.as_str()) {
                    return JSON_LD_CONTEXT.is_match(vocab);
                }
            }
        }
        false
    }

    fn get_json_ld_metadata(doc: &NodeRef, logger: &PerfLogger) -> JsonLdMetadata {
        let mut metadata = JsonLdMetadata::default();
        let scripts = select_descendants(doc, "script");
        for script in scripts {
            if script.attr_value("type").as_deref() != Some("application/ld+json") {
                continue;
            }
            let content = crate::utils::CDATA_STRIP
                .replace_all(script.text_contents().as_str(), "")
                .to_string();
            let parsed: Value = match serde_json::from_str(content.as_str()) {
                Ok(v) => v,
                Err(e) => {
                    d!({
                        add_point_to_span!(
                            logger,
                            GET_METADATA,
                            format!("jsonld_parse_error: {}", e)
                        );
                    });
                    continue;
                }
            };

            let candidate = if let Value::Array(arr) = &parsed {
                arr.iter().find(|v| json_ld_type_matches(v)).cloned()
            } else {
                Some(parsed.clone())
            };

            let candidate_value = match candidate.as_ref() {
                Some(v) => v,
                None => continue,
            };

            if !json_ld_context_matches(candidate_value) {
                continue;
            }

            let article = if json_ld_type_matches(candidate_value) {
                candidate_value.clone()
            } else if let Some(graph) = candidate_value.get("@graph").and_then(|v| v.as_array()) {
                graph
                    .iter()
                    .find(|v| json_ld_type_matches(v))
                    .cloned()
                    .unwrap_or(Value::Null)
            } else {
                Value::Null
            };

            if article.is_null() || !json_ld_type_matches(&article) {
                continue;
            }

            if let (Some(name), Some(headline)) = (
                article.get("name").and_then(|v| v.as_str()),
                article.get("headline").and_then(|v| v.as_str()),
            ) {
                if name != headline {
                    let title = get_clean_title_from_title_tag(doc, logger);
                    let name_matches = text_similarity(name, title.as_str()) > 0.75;
                    let headline_matches = text_similarity(headline, title.as_str()) > 0.75;
                    if headline_matches && !name_matches {
                        metadata.title = Some(headline.trim().to_string());
                    } else {
                        metadata.title = Some(name.trim().to_string());
                    }
                } else {
                    metadata.title = Some(name.trim().to_string());
                }
            } else if let Some(name) = article.get("name").and_then(|v| v.as_str()) {
                metadata.title = Some(name.trim().to_string());
            } else if let Some(headline) = article.get("headline").and_then(|v| v.as_str()) {
                metadata.title = Some(headline.trim().to_string());
            }

            if let Some(author) = article.get("author") {
                if let Some(name) = author.get("name").and_then(|v| v.as_str()) {
                    metadata.by_line = Some(name.trim().to_string());
                } else if let Some(arr) = author.as_array() {
                    let names = arr
                        .iter()
                        .filter_map(|v| v.get("name").and_then(|n| n.as_str()))
                        .map(|s| s.trim().to_string())
                        .collect::<Vec<_>>();
                    if !names.is_empty() {
                        metadata.by_line = Some(names.join(", "));
                    }
                }
            }

            if let Some(publisher) = article.get("publisher") {
                if let Some(name) = publisher.get("name").and_then(|v| v.as_str()) {
                    metadata.site_name = Some(name.trim().to_string());
                }
            }

            if let Some(description) = article.get("description").and_then(|v| v.as_str()) {
                metadata.excerpt = Some(description.trim().to_string());
            }

            if let Some(date_published) = article.get("datePublished").and_then(|v| v.as_str()) {
                metadata.published_time = Some(date_published.trim().to_string());
            }

            break;
        }

        metadata
    }

    pub fn replace_consecutive_brs_with_p(node: &NodeRef) {
        for br in node.select("br").unwrap().collect::<Vec<_>>() {
            // Whether 2 or more <br> elements have been found and replaced with a
            // <p> block.
            let mut replaced = false;
            let br_node = br.as_node();
            let mut next = br_node.next_sibling();

            // If we find a <br> chain, remove the <br>s until we hit another element
            // or non-whitespace. This leaves behind the first <br> in the chain
            // (which will be replaced with a <p> later).

            while next.is_some() {
                next = next_element(next.clone());
                if let Some(n) = next {
                    if n.element_name() == Some("br") {
                        replaced = true;
                        let br_sib = n.next_sibling();
                        n.detach();
                        next = br_sib;
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            }

            // If we removed a <br> chain, replace the remaining <br> with a <p>. Add
            // all sibling nodes as children of the <p> until we hit another <br>
            // chain.
            if replaced {
                let p_element = new_html_element("p");
                br_node.insert_after(p_element.clone());
                br_node.detach();
                next = p_element.next_sibling();
                while next.is_some() {
                    let next_unwraped = next.clone().unwrap();
                    // If we've hit another <br><br>, we're done adding children to this <p>.
                    if next_unwraped.element_name() == Some("br") {
                        let next_e = next_element(next.clone().unwrap().next_sibling());
                        if next_e.is_some() && next_e.unwrap().element_name() == Some("br") {
                            break;
                        }
                    }
                    if !is_phrasing_content(&next_unwraped) {
                        break;
                    }

                    // Otherwise, make this node a child of the new <p>.
                    let sibling = next_unwraped.next_sibling();
                    p_element.append(next_unwraped);
                    next = sibling;
                }

                while p_element.last_child().is_some()
                    && is_whitespace_node(&p_element.last_child().unwrap())
                {
                    p_element.last_child().unwrap().detach();
                }
                if let Some(parent) = p_element.parent() {
                    if parent.element_name() == Some("p") {
                        parent.clone().clone_and_rename_element("div");
                    }
                }
            }
        }
    }

    /// Starting from the passed node, iteratively walk the subtree and remove the class
    /// property value unless the class is present in the classes_to_keep set.
    pub fn clean_classes<S: BuildHasher>(root: &NodeRef, classes_to_keep: &HashSet<String, S>) {
        let mut current = Some(root.clone());
        while let Some(node) = current {
            if let Some(e) = node.as_element() {
                if let Some(class_name) = node.attr_value("class") {
                    let class_names = normalize_text(class_name.as_str())
                        .split(' ')
                        .filter(|cls| {
                            classes_to_keep.contains(*cls) || CLASSES_TO_PRESERVE.contains(*cls)
                        })
                        .collect::<Vec<_>>()
                        .join(" ");
                    if class_names.is_empty() {
                        e.attributes.borrow_mut().remove("class");
                    } else {
                        e.attributes.borrow_mut().insert("class", class_names);
                    }
                }
            }
            current = get_next_node(&node, false);
        }
    }

    pub fn prep_doc(doc: &NodeRef, remove_style_tags: bool, logger: &PerfLogger) {
        start_span!(logger, PREP_DOC);
        remove_tags_with_selector(doc, "script");
        add_point_to_span_str!(logger, PREP_DOC, "remove_scripts_done");
        remove_tags_with_selector(doc, "noscript");
        add_point_to_span_str!(logger, PREP_DOC, "remove_noscripts_done");
        remove_comment_nodes(doc);
        add_point_to_span_str!(logger, PREP_DOC, "remove_comments_done");
        if remove_style_tags {
            remove_tags_with_selector(doc, "style");
            add_point_to_span_str!(logger, PREP_DOC, "remove_style_done");
        }
        replace_consecutive_brs_with_p(doc);
        add_point_to_span_str!(logger, PREP_DOC, "replace_consecutive_brs_with_p_done");
        rename_tags_with_selector(doc, "font", "span");
        add_point_to_span_str!(logger, PREP_DOC, "rename_font_tags_to_span_done");
        end_span!(logger, PREP_DOC);
    }

    pub fn get_metadata(doc: &NodeRef, logger: &PerfLogger) -> Metadata {
        start_span!(logger, GET_METADATA);

        let mut values: HashMap<String, String> = HashMap::new();

        // for logging purposes
        // track how much meta tags we checked
        // and how many of them were useless
        let mut num_of_meta_checked = 0;
        let mut num_of_meta_skipped = 0;

        let mut name: String;
        for meta_element in doc.select("meta").unwrap() {
            num_of_meta_checked += 1;
            let e_name = meta_element.as_node().attr_value("name");
            let e_property = meta_element.as_node().attr_value("property");
            let e_content = meta_element.as_node().attr_value("content");
            if e_content.is_none() {
                d!({
                    num_of_meta_skipped += 1;
                });
                continue;
            }

            let content = e_content.unwrap().trim().to_string();
            let mut property_matches = false;
            if let Some(property) = e_property {
                for cap in META_PROPERTY_PATTERN.captures_iter(property.as_str()) {
                    name = cap.get(0).unwrap().as_str().to_lowercase().replace(' ', "");
                    values.insert(name, content.clone());
                    property_matches = true;
                }
            }
            if let Some(e_n) = e_name {
                if !property_matches && META_NAME_PATTERN.is_match(e_n.as_str()) {
                    name = e_n.clone();
                    name = name
                        .trim()
                        .to_lowercase()
                        .replace('.', ":")
                        .replace(' ', "");
                    values.insert(name, content.clone());
                }
            }
        }
        let jsonld = get_json_ld_metadata(doc, logger);

        let mut title = jsonld
            .title
            .or_else(|| values.get("dc:title").cloned())
            .or_else(|| values.get("dcterm:title").cloned())
            .or_else(|| values.get("og:title").cloned())
            .or_else(|| values.get("weibo:article:title").cloned())
            .or_else(|| values.get("weibo:webpage:title").cloned())
            .or_else(|| values.get("title").cloned())
            .or_else(|| values.get("twitter:title").cloned())
            .or_else(|| values.get("parsely-title").cloned())
            .unwrap_or_else(|| get_clean_title_from_title_tag(doc, logger));

        let article_author = values
            .get("article:author")
            .filter(|v| Url::parse(v.as_str()).is_err())
            .cloned();

        let mut by_line = jsonld
            .by_line
            .or_else(|| values.get("dc:creator").cloned())
            .or_else(|| values.get("dcterm:creator").cloned())
            .or_else(|| values.get("author").cloned())
            .or_else(|| values.get("parsely-author").cloned())
            .or(article_author)
            .unwrap_or_default();

        let mut excerpt = jsonld
            .excerpt
            .or_else(|| values.get("dc:description").cloned())
            .or_else(|| values.get("dcterm:description").cloned())
            .or_else(|| values.get("og:description").cloned())
            .or_else(|| values.get("weibo:article:description").cloned())
            .or_else(|| values.get("weibo:webpage:description").cloned())
            .or_else(|| values.get("description").cloned())
            .or_else(|| values.get("twitter:description").cloned())
            .unwrap_or_default();

        let mut sitename = jsonld
            .site_name
            .or_else(|| values.get("og:site_name").cloned())
            .unwrap_or_default();

        let mut published_time = jsonld
            .published_time
            .or_else(|| values.get("article:published_time").cloned())
            .or_else(|| values.get("parsely-pub-date").cloned())
            .unwrap_or_default();

        title = unescape_html_entities(title.as_str());
        by_line = unescape_html_entities(by_line.as_str());
        excerpt = unescape_html_entities(excerpt.as_str());
        sitename = unescape_html_entities(sitename.as_str());
        published_time = unescape_html_entities(published_time.as_str());
        add_point_to_span!(
            logger,
            GET_METADATA,
            format!(
                "parsed {} meta tags and skipped {}",
                num_of_meta_checked, num_of_meta_skipped
            )
        );
        let metadata = Metadata {
            title,
            by_line,
            sitename,
            excerpt,
            published_time,
        };

        end_span!(logger, GET_METADATA);

        metadata
    }

    pub(crate) fn get_clean_title_from_title_tag(doc: &NodeRef, logger: &PerfLogger) -> String {
        let mut curr_title = String::new();
        let mut origin_title = String::new();
        if let Ok(title_element) = doc.select_first("title") {
            origin_title = title_element.text_contents().trim().to_string();
            curr_title = origin_title.clone();
        }

        if curr_title.is_empty() {
            if let Ok(title_element) = doc.select_first("#title") {
                origin_title = get_normalized_text_content(title_element.as_node(), logger)
                    .trim()
                    .to_string();
                curr_title = origin_title.clone();
            }
        }

        let mut title_had_hierarchical_separators = false;

        // If there's a separator in the title, first remove the final part
        if TITLE_SEPARATOR.is_match(curr_title.as_str()) {
            title_had_hierarchical_separators =
                TITLE_HIERARCHICAL_SEPARATOR.is_match(curr_title.as_str());

            if let Some(last_sep) = TITLE_SEPARATOR.find_iter(origin_title.as_str()).last() {
                curr_title = origin_title[..last_sep.start()].to_string();
            }

            if word_count(curr_title.as_str()) < 3 {
                curr_title = TITLE_STRIP_PREFIX
                    .replace(origin_title.as_str(), "")
                    .to_string();
            }
        } else if curr_title.contains(": ") {
            // Check if we have a heading containing this exact string, so we
            // could assume it's the full title.
            let mut match_headings = test_any_node_by_selector(doc, "h1", |node| {
                node.text_contents().trim() == curr_title
            });

            if !match_headings {
                match_headings = test_any_node_by_selector(doc, "h2", |node| {
                    node.text_contents().trim() == curr_title
                });
            }
            if !match_headings {
                if let Some(pos) = origin_title.rfind(':') {
                    curr_title = origin_title[pos + 1..].to_string();
                }

                // If the title is now too short, try the first colon instead:
                if word_count(curr_title.as_str()) < 3 {
                    if let Some(pos) = origin_title.find(':') {
                        curr_title = origin_title[pos + 1..].to_string();
                    }
                } else if let Some(pos) = origin_title.find(':') {
                    // But if we have too many words before the colon there's something weird
                    // with the titles and the H tags so let's just use the original title instead
                    let prefix = origin_title[..pos].to_string();
                    if word_count(prefix.as_str()) > 5 {
                        curr_title = origin_title.clone();
                    }
                }
            }
        } else if curr_title.len() > 150 || curr_title.len() < 15 {
            let h_ones: Vec<_> = doc.select("h1").unwrap().collect();
            if h_ones.len() == 1 {
                curr_title = h_ones[0].text_contents().trim().to_string();
            }
        }

        curr_title = normalize_text(curr_title.as_str());

        let curr_title_word_count = word_count(curr_title.as_str());
        if curr_title_word_count <= 4 {
            if !title_had_hierarchical_separators {
                curr_title = origin_title
            } else {
                let tmp = TITLE_SEPARATOR
                    .replace(origin_title.as_str(), "")
                    .to_string();
                if word_count(tmp.as_str()) - 1 != curr_title_word_count {
                    curr_title = origin_title
                }
            }
        }

        curr_title.trim().to_string()
    }

    // tests moved to end of file
}

use extractor_utils::*;

pub struct Extractor<'a> {
    pub html: &'a str,
    pub doc_uri: String,
    pub options: ExtractOptions,
    logger: PerfLogger,
}

impl Extractor<'_> {
    pub fn new<'a>(
        html: &'a str,
        doc_uri: String,
        options: ExtractOptions,
    ) -> Extractor<'a> {
        Extractor {
            html,
            doc_uri,
            options,
            logger: create_perf_logger(),
        }
    }

    pub fn extract(self) -> Product {
        let logger = &self.logger;
        start_span!(logger, EXTRACT);
        let document = parse_html(self.html);
        let mut score_store = NodeScoreStore::default();
        unwrap_noscript_images(&document);
        let metadata = get_metadata(&document, logger);
        let article_title = if !metadata.title.trim().is_empty() {
            metadata.title.clone()
        } else {
            get_clean_title_from_title_tag(&document, logger)
        };

        prep_doc(&document, self.options.remove_style_tags, logger);
        let fallback_html = self.html;
        let initial_by_line = if metadata.by_line.trim().is_empty() {
            None
        } else {
            Some(metadata.by_line.clone())
        };
        let content_data = article::get_content(
            document.clone(),
            self.options.clone(),
            article_title,
            initial_by_line,
            &mut score_store,
            &self.logger,
        );

        if content_data.is_none() {
            let fallback_doc = parse_html(fallback_html);
            if let Ok(body) = fallback_doc.select_first("body") {
                let fallback_page = new_html_element("div");
                if let Some(e) = fallback_page.as_element() {
                    e.attributes
                        .borrow_mut()
                        .insert("id", "readability-page-1".to_string());
                    e.attributes
                        .borrow_mut()
                        .insert("class", "page".to_string());
                }
                move_children(body.as_node(), &fallback_page);
                self.post_process_content(&fallback_page, &document);
                let fallback_container = new_html_element("div");
                fallback_container.append(fallback_page);
                let mut excerpt = metadata.excerpt;
                if excerpt.trim().is_empty() {
                    if let Ok(p) = fallback_container.select_first("p") {
                        excerpt = p.as_node().text_contents().trim().to_string();
                    }
                }
                return Product {
                    title: metadata.title,
                    content: Some(fallback_container),
                    by_line: metadata.by_line,
                    dir: String::new(),
                    sitename: metadata.sitename,
                    excerpt,
                    published_time: metadata.published_time,
                    score_store,
                };
            }
            return Product {
                title: metadata.title,
                content: None,
                by_line: metadata.by_line,
                dir: String::new(),
                sitename: metadata.sitename,
                excerpt: metadata.excerpt,
                published_time: metadata.published_time,
                score_store,
            };
        }

        let content_data = content_data.unwrap();
        let mut excerpt = metadata.excerpt;
        if excerpt.trim().is_empty() {
            if let Ok(p) = content_data.content.select_first("p") {
                excerpt = p.as_node().text_contents().trim().to_string();
            }
        }
        self.post_process_content(&content_data.content, &document);

        let product = Product {
            title: metadata.title,
            content: Some(content_data.content),
            by_line: if !metadata.by_line.is_empty() {
                metadata.by_line
            } else {
                content_data.by_line
            },
            dir: content_data.article_dir,
            sitename: metadata.sitename,
            excerpt,
            published_time: metadata.published_time,
            score_store,
        };

        end_span!(logger, EXTRACT);
        product
    }

    fn post_process_content(&self, content_node: &NodeRef, document: &NodeRef) {
        let mut base_path = String::new();
        if let Ok(base) = document.select_first("base") {
            if let Some(href) = base.as_node().attr_value("href") {
                base_path = href;
            }
        }
        replace_relative_urls_with_absolute(
            content_node,
            self.doc_uri.as_str(),
            base_path.as_str(),
        );
        simplify_nested_elements(content_node);

        if !self.options.keep_classes {
            clean_classes(content_node, &self.options.classes_to_preserve);
        }
        normalize_text_nodes(content_node);
        cleanup_readability_p_wrappers(content_node);
    }
}

#[cfg(test)]
mod tests {
    use super::extractor_utils::*;
    use crate::logging::logger::PerfLogger;
    use crate::parser::parse_html;
    use crate::utils::count_elements;
    use serde_json::json;
    use std::collections::HashSet;

    #[test]
    fn test_replaces_consecutive_brs_with_p_element() {
        const TEST_INPUT: &str = r###"<!doctype html><html><head>
<title>Example Domain</title>
</head>
<body>
<div>foo<br>bar<br> <br><br>abc</div>
</body>
</html>"###;
        let doc = parse_html(TEST_INPUT);
        assert_eq!(count_elements(&doc, "br"), 4);
        assert_eq!(count_elements(&doc, "p"), 0);
        assert_eq!(count_elements(&doc, "div"), 1);
        replace_consecutive_brs_with_p(&doc);
        assert_eq!(count_elements(&doc, "br"), 1);
        assert_eq!(count_elements(&doc, "p"), 1);
        assert_eq!(count_elements(&doc, "div"), 1);
    }

    #[test]
    fn test_replaces_consecutive_brs_with_p_element_contained_in_another_p_element() {
        const TEST_INPUT: &str = r###"<!doctype html><html><head>
<title>Example Domain</title>
</head>
<body>
<div>foo<br />bar<p><br /><br /><br />abc<br /><br /></p></div>
</body>
</html>"###;
        let doc = parse_html(TEST_INPUT);
        assert_eq!(count_elements(&doc, "br"), 6);
        assert_eq!(count_elements(&doc, "p"), 1);
        assert_eq!(count_elements(&doc, "div"), 1);
        replace_consecutive_brs_with_p(&doc);
        assert_eq!(count_elements(&doc, "br"), 1);
        assert_eq!(count_elements(&doc, "p"), 2);
        assert_eq!(count_elements(&doc, "div"), 2);
    }

    #[test]
    fn test_clean_classes() {
        const TEST_INPUT: &str = r###"<!doctype html><html><head>
<title>Example Domain</title>
</head>
<body>
<div class="class1">
<table class="class2">
<tr><th>Firstname</th>
<p class="keep1">Text Here</p>
<p>Another P</p>
</tr>
<tr>
<td class="keep2">Jill</td>
</tr>
</table>
</div>
</body>
</html>"###;

        let doc = parse_html(TEST_INPUT);
        assert_eq!(doc.select(".class1").unwrap().count(), 1);
        assert_eq!(doc.select(".class2").unwrap().count(), 1);
        assert_eq!(doc.select(".keep1").unwrap().count(), 1);
        assert_eq!(doc.select(".keep2").unwrap().count(), 1);
        let mut classes_to_keep: HashSet<String> = HashSet::new();
        classes_to_keep.insert(String::from("keep1"));
        classes_to_keep.insert(String::from("keep2"));
        clean_classes(&doc, &classes_to_keep);
        assert_eq!(doc.select(".class1").unwrap().count(), 0);
        assert_eq!(doc.select(".class2").unwrap().count(), 0);
        assert_eq!(doc.select(".keep1").unwrap().count(), 1);
        assert_eq!(doc.select(".keep2").unwrap().count(), 1);
    }

    #[test]
    fn test_json_ld_context_matches_string() {
        let v = json!({
            "@context": "https://schema.org"
        });
        assert!(json_ld_context_matches(&v));
    }

    #[test]
    fn test_json_ld_context_matches_object_vocab() {
        let v = json!({
            "@context": {
                "@vocab": "http://schema.org/"
            }
        });
        assert!(json_ld_context_matches(&v));
    }

    #[test]
    fn test_get_metadata_from_meta_tags() {
        let html = r#"
            <html><head>
                <meta property="og:title" content="My Title" />
                <meta name="author" content="Jane Doe" />
                <meta name="description" content="Short desc" />
                <meta property="og:site_name" content="Example Site" />
            </head><body></body></html>
        "#;
        let doc = parse_html(html);
        let logger = PerfLogger::new(vec![]);
        let metadata = get_metadata(&doc, &logger);
        assert_eq!(metadata.title, "My Title");
        assert_eq!(metadata.by_line, "Jane Doe");
        assert_eq!(metadata.excerpt, "Short desc");
        assert_eq!(metadata.sitename, "Example Site");
    }

    #[test]
    fn test_get_clean_title_from_title_tag_with_separator() {
        let html = r#"<html><head><title>Article Title Example | Site Name</title></head><body></body></html>"#;
        let doc = parse_html(html);
        let logger = PerfLogger::new(vec![]);
        let title = get_clean_title_from_title_tag(&doc, &logger);
        assert_eq!(title, "Article Title Example | Site Name");
    }
}
