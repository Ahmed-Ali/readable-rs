mod cleaner;
mod scorer;
mod utils;

use crate::logging::logger::*;
use crate::logging::logging_defs::*;
use crate::models::ExtractOptions;
use crate::node_ext::NodeScoreStore;
use crate::parser::{NodeExt, NodeRef, new_html_element, parse_html};
use crate::utils::*;
use std::collections::HashSet;
use std::sync::LazyLock;
use utils::*;

pub static ALTER_TO_DIV_EXCEPTIONS: LazyLock<HashSet<&'static str>> =
    LazyLock::new(|| HashSet::from(["div", "article", "section", "p", "ol", "ul"]));

const DEFAULT_CONTENT_ID: &str = "readability-page-1";
const DEFAULT_CONTENT_CLASS: &str = "page";

/// The result of the article-content extraction phase.
pub struct ContentData {
    /// The extracted content subtree, wrapped in a `<div id="readability-page-1">`.
    pub content: NodeRef,
    /// The byline / author string detected during the cleaning pass, if any.
    pub by_line: String,
    /// The text-direction (`"ltr"` / `"rtl"`) inferred from the top
    /// candidate's ancestor `dir` attributes.  Empty when no `dir` was found.
    pub article_dir: String,
}

struct ContentParser<'a> {
    document: NodeRef,
    logger: &'a PerfLogger,
    options: ExtractOptions,
    debug: bool,
    article_title: String,
    initial_by_line: Option<String>,
}

impl<'a> ContentParser<'a> {
    fn new(
        document: NodeRef,
        article_title: String,
        options: ExtractOptions,
        initial_by_line: Option<String>,
        logger: &PerfLogger,
    ) -> ContentParser<'_> {
        let debug = options.debug;
        ContentParser {
            document,
            logger,
            options,
            debug,
            article_title,
            initial_by_line,
        }
    }

    fn remove_nodes_conditionally(
        &self,
        article_content: &NodeRef,
        tags_to_remove: &[&str],
        store: &NodeScoreStore,
    ) {
        if self.options.clean_conditionally {
            let link_density_modifier = self.options.link_density_modifier;
            for tag_name in tags_to_remove {
                let is_list = tag_name == &"ul" || tag_name == &"ol";
                remove_nodes(article_content, tag_name, |node, tag| {
                    is_safe_to_remove_node(
                        node,
                        tag,
                        is_list,
                        link_density_modifier,
                        self.logger,
                        store,
                    )
                });
            }
        }
    }

    fn prep_article(
        &self,
        article_content: &NodeRef,
        logger: &PerfLogger,
        store: &mut NodeScoreStore,
    ) {
        clean_presentation_styles(article_content);

        // Check for data tables before we continue, to avoid removing items in
        // those tables, which will often be isolated even though they're
        // visually linked to other content-ful elements (text, images, etc.).
        mark_data_tables_in_node(article_content, store);

        fix_lazy_images(article_content);

        // Clean out junk from the article content
        self.remove_nodes_conditionally(article_content, &["form", "fieldset"], store);

        for t in &["object", "embed", "iframe"] {
            remove_nodes(article_content, t, |node, tag| {
                !is_possibly_useful_video_node(node, tag)
            });
        }

        // Remove known gallery widgets that shouldn't appear in reader view.
        remove_nodes(article_content, "div", |node, _| {
            node.attr_value("data-scald-gallery").is_some()
        });

        for t in &["footer", "link", "aside"] {
            remove_nodes(article_content, t, |_, _| true);
        }

        // Clean out elements with little content that have "share" in their id/class combinations from final top candidates,
        // which means we don't remove the top candidates even they have "share".
        for n in article_content.children() {
            remove_matched_nodes(&n, |node, match_str| {
                let is_share_element = SHARE_ELEMENTS_REGEX.is_match(match_str);
                let smaller_than_threshold =
                    node.text_contents().chars().count() < self.options.char_threshold as usize;
                is_share_element && smaller_than_threshold
            });
        }

        for t in &["input", "textarea", "select", "button"] {
            remove_nodes(article_content, t, |_, _| true);
        }

        for t in &["h1", "h2"] {
            remove_nodes(article_content, t, |node, _| {
                get_class_and_id_weight(node) < 0
            });
        }

        // Do these last as the previous stuff may have removed junk
        // that will affect these
        self.remove_nodes_conditionally(article_content, &["table", "ul"], store);
        self.remove_nodes_conditionally(article_content, &["div"], store);
        for t in &["div", "section"] {
            remove_nodes(article_content, t, |node, _| {
                let inner = get_normalized_text_content(node, logger);
                matches_ad_or_loading(inner.as_str())
            });
        }
        rename_tags_with_selector(article_content, "h1", "h2");

        // Remove extra paragraphs
        remove_nodes(article_content, "p", |node, _| {
            let img = select_descendants(node, "img").len();
            let embed = select_descendants(node, "embed").len();
            let object = select_descendants(node, "object").len();
            // At this point, nasty iframes have been removed, only remain embedded video ones.
            let iframe = select_descendants(node, "iframe").len();
            let total_count = img + embed + object + iframe;
            total_count == 0 && node.text_contents().is_empty()
        });

        apply(article_content, &["br"], |br, _| {
            let next = next_element(Some(br.clone()));
            if next.is_some() && next.unwrap().element_name() == Some("p") {
                br.detach();
            }
        });

        apply(article_content, &["table"], |table, _| {
            let tbody = if contains_single_tag_in_element(table, "tbody") {
                table.first_element_child().unwrap()
            } else {
                table.clone()
            };

            if contains_single_tag_in_element(&tbody, "tr") {
                let row = tbody.first_element_child().unwrap();
                if contains_single_tag_in_element(&row, "td") {
                    let cell = row.first_element_child().unwrap();
                    let new_tag_name = if test_all_siblings(cell.first_child(), is_phrasing_content)
                    {
                        "p"
                    } else {
                        "div"
                    };

                    let cell = cell.clone_and_rename_element(new_tag_name);
                    table.insert_after(cell);
                    table.detach();
                }
            }
        });

        // For all elements
        // remove any element that is considered empty
        // this can be as a result of a previous clean up process
        article_content
            .descendants()
            .filter(|n| is_empty_node(n, logger))
            .for_each(|n| {
                if let Some(id) = n.attr_value("id") {
                    // here things are interesting
                    if let Some(name) = n.element_name() {
                        // 1- if the node is an "a", then we let that to the links resolution to deal with
                        // 2- if it a span element, spans are light weight element, let it be
                        // 3- otherwise, let's replace it with an empty span that holds the same id
                        if name != "a" && name != "span" {
                            let span = new_html_element("span");
                            let e = span.as_element().unwrap();
                            e.attributes.borrow_mut().insert("id", id);
                            n.insert_after(span);
                            n.detach();
                        }
                    }
                } else {
                    // none else cares about this node
                    // detach it from the nodes tree
                    n.detach();
                }
            });
    }

    fn get_content_node(&self) -> NodeRef {
        let content_node = self.document.select_first("body");
        if content_node.is_err() {
            if self.debug {
                let logger = self.logger;
                add_point_to_span_str!(logger, PARSE_CONTENT, "missing_body");
            }
            return self.document.clone();
        }

        let content_node = content_node.unwrap();
        content_node.as_node().clone()
    }

    fn update_or_create_top_candidate_node(
        &self,
        article_content: &NodeRef,
        top_candidate: &NodeRef,
        had_to_create_top_candidate_node: bool,
        _logger: &PerfLogger,
    ) {
        if had_to_create_top_candidate_node {
            // We already created a fake div thing, and there wouldn't have been any siblings left
            // for the previous loop, so there's no point trying to create a new div, and then
            // move all the children over. Just assign IDs and class names here. No need to append
            // because that already happened anyway.
            top_candidate
                .as_element()
                .unwrap()
                .attributes
                .borrow_mut()
                .insert("id", String::from(DEFAULT_CONTENT_ID));
            top_candidate
                .as_element()
                .unwrap()
                .attributes
                .borrow_mut()
                .insert("class", String::from(DEFAULT_CONTENT_CLASS));
        } else {
            let div = new_html_element("div");
            div.as_element()
                .unwrap()
                .attributes
                .borrow_mut()
                .insert("id", String::from(DEFAULT_CONTENT_ID));
            div.as_element()
                .unwrap()
                .attributes
                .borrow_mut()
                .insert("class", String::from(DEFAULT_CONTENT_CLASS));
            move_children(article_content, &div);
            article_content.append(div);
        }
    }

    pub fn parse(&mut self, store: &mut NodeScoreStore) -> Option<ContentData> {
        let logger = &self.logger;
        start_span!(logger, PARSE_CONTENT);
        let body_content = self.get_content_node();
        add_point_to_span_str!(
            logger,
            PARSE_CONTENT,
            "get_content_node_and_its_serialized_html_begin"
        );

        let (mut doc_node, content_cache) = (self.document.clone(), self.document.to_string());
        let mut by_line: Option<String> = self.initial_by_line.clone();
        let mut attempts: Vec<(NodeRef, usize)> = vec![];
        let mut article_dir: Option<String> = None;
        loop {
            let elements_to_score = cleaner::clean(
                &doc_node,
                &mut by_line,
                self.options.strip_unlikelys,
                self.article_title.as_str(),
                logger,
            );
            let scoring_res = scorer::score_elements(
                &elements_to_score,
                store,
                self.options.weight_classes,
                self.options.n_top_candidates as usize,
                &body_content,
                logger,
            );
            // Now that we have the top candidate, look through its siblings for content
            // that might also be related. Things like preambles, content split by ads
            // that we removed, etc.
            let mut article_content = new_html_element("div");
            let top_candidate = scoring_res.top_candidate;
            // Keep potential top candidate's parent node to try to get text direction of it later.
            let parent_of_top_candidate = top_candidate.parent();

            if let Some(p) = parent_of_top_candidate.clone() {
                for sibling in p.element_children() {
                    let append = if sibling == top_candidate {
                        true
                    } else {
                        should_append_sibling(&sibling, &top_candidate, logger, store)
                    };

                    if append {
                        if let Some(sibling_name) = sibling.element_name() {
                            let sibling_name = sibling_name.to_lowercase();
                            if !ALTER_TO_DIV_EXCEPTIONS.contains(sibling_name.as_str()) {
                                article_content.append(sibling.clone_and_rename_element("div"));
                            } else {
                                article_content.append(sibling);
                            }
                        } else {
                            article_content.append(sibling);
                        }
                    }
                }
            }

            self.prep_article(&article_content, logger, store);

            self.update_or_create_top_candidate_node(
                &article_content,
                &top_candidate,
                scoring_res.had_to_create_top_candidate_node,
                logger,
            );

            // Now that we've gone through the full algorithm, check to see if
            // we got any meaningful content. If we didn't, we may need to re-run
            // this method with different flags set. This gives us a higher likelihood of
            // finding the content, and the sieve approach gives us a higher likelihood of
            // finding the -right- content.
            let mut parse_successfully = true;
            let normalized_text = get_normalized_text_content(&article_content, logger);
            let text_length = normalized_text.chars().count();
            if text_length < self.options.char_threshold as usize {
                parse_successfully = false;
                doc_node = parse_html(content_cache.as_str());

                attempts.push((article_content.clone(), text_length));
                if self.options.strip_unlikelys {
                    self.options.strip_unlikelys = false;
                } else if self.options.weight_classes {
                    self.options.weight_classes = false;
                } else if self.options.clean_conditionally {
                    self.options.clean_conditionally = false;
                } else {
                    // No luck with all flags off,
                    // just return the longest text we found during the different loops
                    attempts.sort_by(|lhs, rhs| rhs.1.cmp(&lhs.1));

                    if let Some(attmpt) = attempts.first() {
                        if attmpt.1 == 0 {
                            let fallback_text = article_content.text_contents();
                            if !fallback_text.trim().is_empty() {
                                parse_successfully = true;
                            } else {
                                d!({
                                    eprintln!(
                                        "Failed to get readable article content with all flags turned off: {}",
                                        self.article_title
                                    )
                                });
                                return None;
                            }
                        } else {
                            article_content = attmpt.0.clone();
                            parse_successfully = true;
                        }
                    } else {
                        d!({
                            eprintln!(
                                "Failed to get readable article content with all flags turned off: {}",
                                self.article_title
                            )
                        });
                        return None;
                    }
                }
            }

            if parse_successfully {
                let mut ancestors = vec![top_candidate];
                if let Some(p) = parent_of_top_candidate {
                    ancestors.push(p.clone());
                    ancestors.append(&mut get_node_ancestors(&p, DEFAULT_MAX_ANCESTORS_DEPTH));
                }

                for n in ancestors {
                    if let (Some(_), Some(dir)) = (n.element_name(), n.attr_value("dir")) {
                        article_dir = Some(dir);
                        break;
                    }
                }

                return Some(ContentData {
                    content: article_content,
                    by_line: by_line.unwrap_or_default(),
                    article_dir: article_dir.unwrap_or_default(),
                });
            }
        }
    }
}

/// Run the full content-extraction pipeline on `document`.
///
/// Returns `Some(ContentData)` if a content subtree with enough text was
/// found, or `None` if all retry strategies (progressively disabling
/// `strip_unlikelys`, `weight_classes`, and `clean_conditionally`) were
/// exhausted without success.
pub fn get_content(
    document: NodeRef,
    options: ExtractOptions,
    article_title: String,
    initial_by_line: Option<String>,
    store: &mut NodeScoreStore,
    logger: &PerfLogger,
) -> Option<ContentData> {
    let mut parser = ContentParser::new(document, article_title, options, initial_by_line, logger);

    parser.parse(store)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::logging::logger::PerfLogger;
    use std::panic::{AssertUnwindSafe, catch_unwind};

    fn new_logger() -> PerfLogger {
        PerfLogger::new(vec![])
    }

    #[test]
    fn parse_no_body_does_not_panic_with_debug_false() {
        let doc = new_html_element("div");
        let logger = new_logger();
        let mut options = ExtractOptions::default();
        options.debug = false;
        let mut parser = ContentParser::new(doc, "title".to_string(), options, None, &logger);
        let mut store = NodeScoreStore::default();
        let res = catch_unwind(AssertUnwindSafe(|| parser.parse(&mut store)));
        assert!(res.is_ok());
    }

    #[test]
    fn parse_no_body_does_not_panic_with_debug_true() {
        let doc = new_html_element("div");
        let logger = new_logger();
        let mut options = ExtractOptions::default();
        options.debug = true;
        let mut parser = ContentParser::new(doc, "title".to_string(), options, None, &logger);
        let mut store = NodeScoreStore::default();
        let res = catch_unwind(AssertUnwindSafe(|| parser.parse(&mut store)));
        assert!(res.is_ok());
    }
}
