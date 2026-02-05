use crate::logging::logger::*;
use crate::logging::logging_defs::*;
use crate::parser::{NodeExt, NodeRef, new_html_element, parse_html};

use regex::Regex;
use std::collections::{HashMap, HashSet};
use std::sync::LazyLock;

/// HTML phrasing-content tag names per the [HTML spec](https://html.spec.whatwg.org/multipage/dom.html#phrasing-content).
pub static PHRASING_ELEMENTS: LazyLock<HashSet<&'static str>> = LazyLock::new(|| {
    HashSet::from([
        "abbr", "audio", "b", "bdo", "br", "button", "cite", "code", "data", "datalist", "dfn",
        "em", "embed", "i", "img", "input", "kbd", "label", "mark", "math", "meter",
        "noscript", "object", "output", "progress", "q", "ruby", "samp", "script", "select",
        "small", "span", "strong", "sub", "sup", "textarea", "time", "var", "wbr",
    ])
});

/// HTML attributes that are purely presentational and are stripped during
/// the cleanup phase.
pub static PRESENTATIONAL_ATTRIBUTES: LazyLock<Vec<&'static str>> = LazyLock::new(|| {
    vec![
        "align",
        "background",
        "bgcolor",
        "border",
        "cellpadding",
        "cellspacing",
        "frame",
        "hspace",
        "rules",
        "style",
        "valign",
        "vspace",
    ]
});

/// Element tags that historically accepted (now-deprecated) `width`/`height`
/// attributes.  Those attributes are stripped when such elements are renamed.
pub static DEPRECATED_SIZE_ATTRIBUTE_ELEMS: LazyLock<HashSet<&'static str>> =
    LazyLock::new(|| HashSet::from(["table", "th", "td", "hr", "pre"]));

/// Void (self-closing) HTML elements — they have no closing tag and therefore
/// no children.  Used to avoid treating empty void elements as "empty nodes"
/// that should be pruned.
pub static SELF_CLOSING_TAGS: LazyLock<HashSet<&'static str>> = LazyLock::new(|| {
    HashSet::from([
        "area", "base", "br", "col", "embed", "hr", "img", "input", "link", "param", "source",
        "track", "wbr",
    ])
});

/// Matches a string that ends with a non-whitespace character (i.e. has
/// visible content).
pub static HAS_CONTENT: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"\S$").unwrap());

/// Class / id tokens that suggest a node is *content* (article body, blog
/// post, etc.).
pub static POSITIVE_CLASSES_AND_IDS: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"(?i)article|body|content|entry|hentry|h-entry|main|page|pagination|post|text|blog|story").unwrap()
});
/// Class / id tokens that suggest a node is *non-content* (ads, sidebars,
/// navigation, footers, etc.).
pub static NEGATIVE_CLASSES_AND_IDS: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"(?i)-ad-|hidden|^hid$| hid$| hid |^hid |banner|combx|comment|com-|contact|footer|gdpr|masthead|media|meta|outbrain|promo|related|scroll|share|shoutbox|sidebar|skyscraper|sponsor|shopping|tags|widget").unwrap()
});
pub static VIDEO_ATTRS_REGEX: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"(?i)//(www\.)?((dailymotion|youtube|youtube-nocookie|player\.vimeo|v\.qq|bilibili|live\.bilibili)\.com|(archive|upload\.wikimedia)\.org|player\.twitch\.tv)").unwrap()
});
pub static SHARE_ELEMENTS_REGEX: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"(?i)(\b|_)(share|sharedaddy)(\b|_)").unwrap());
pub static UNLIKELY_CANDIDATES_REGEX: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"(?i)-ad-|ai2html|banner|breadcrumbs|combx|comment|community|cover-wrap|disqus|extra|footer|gdpr|header|legends|menu|related|remark|replies|rss|shoutbox|sidebar|skyscraper|social|sponsor|supplemental|ad-break|agegate|pagination|pager|popup|yom-remote").unwrap()
});
pub static MAYBE_A_CANDIDATE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"(?i)and|article|body|column|content|main|mathjax|shadow").unwrap());
pub static TOKENIZE_REGEX: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"[^A-Za-z0-9_]+").unwrap());
pub static B64_DATA_URL: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"(?i)^data:\s*([^\s;,]+)\s*;\s*base64\s*,").unwrap());
pub static SRCSET_URL: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"(\S+)(\s+[\d.]+[xw])?(\s*(?:,|$))").unwrap());
pub static COMMA_REGEX: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r"\u002C|\u060C|\uFE50|\uFE10|\uFE11|\u2E41|\u2E34|\u2E32|\uFF0C").unwrap()
});
pub static UNESCAPE_NAMED_ENTITIES: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"&(?:quot|amp|apos|lt|gt);").unwrap());
pub static UNESCAPE_NUMERIC_ENTITIES: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"&#(?:x([0-9a-fA-F]+)|([0-9]+));").unwrap());
pub static IMAGE_EXTENSION: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"(?i)\.(jpg|jpeg|png|webp)").unwrap());
pub static SRCSET_EXTENSION: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"\.(jpg|jpeg|png|webp)\s+\d").unwrap());
pub static SRC_EXTENSION: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"^\s*\S+\.(jpg|jpeg|png|webp)\S*\s*$").unwrap());
pub static SENTENCE_END: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"\.( |$)").unwrap());
pub static CDATA_STRIP: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"<!\[CDATA\[|\]\]>").unwrap());
pub static UNLIKELY_ROLES: LazyLock<HashSet<&'static str>> = LazyLock::new(|| {
    HashSet::from([
        "menu",
        "menubar",
        "complementary",
        "navigation",
        "alert",
        "alertdialog",
        "dialog",
    ])
});

/// HTML attributes that can serve as anchor targets.  Nodes carrying any of
/// these are kept as placeholders even when otherwise empty.
pub const REFERENCING_ATTRIBUTES: &[&str] = &["id", "name"];

/// Sentinel value meaning "walk all the way to the root" when passed as
/// `max_depth` to ancestor-lookup helpers.
pub const DEFAULT_MAX_ANCESTORS_DEPTH: i16 = 0;

/// Default depth limit for ancestor lookups that are used as quick
/// heuristic checks (e.g. "is this node inside a `<table>`?").
pub const DEFAULT_MAX_ANCESTORS_LOOKUP_DEPTH: i16 = 3;

/// Join two optional strings with a separator, omitting the separator when
/// either side is `None`.
///
/// ```rust
/// use readable_rs::shared_utils::*; // for illustration; concat_optionals is internal
/// // concat_optionals(Some("a".into()), Some("b".into()), " ") == "a b"
/// // concat_optionals(None,            Some("b".into()), " ") == "b"
/// // concat_optionals(Some("a".into()), None,            " ") == "a"
/// // concat_optionals(None,            None,            " ") == ""
/// ```
pub fn concat_optionals(l: Option<String>, r: Option<String>, sep: &str) -> String {
    match (l, r) {
        (Some(l), Some(r)) => format!("{}{}{}", l, sep, r),
        (Some(l), None) => l,
        (None, Some(r)) => r,
        (None, None) => String::new(),
    }
}

/// Detach every descendant of `node` that matches the CSS `selector`.
pub fn remove_tags_with_selector(node: &NodeRef, selector: &str) {
    for n in select_descendants(node, selector) {
        n.detach();
    }
}

/// Return all descendants of `node` that match `selector`, excluding `node`
/// itself.  An invalid selector returns an empty `Vec` rather than panicking.
pub fn select_descendants(node: &NodeRef, selector: &str) -> Vec<NodeRef> {
    match node.select(selector) {
        Ok(iter) => iter
            .filter_map(|e| {
                let n = e.as_node();
                if n == node { None } else { Some(n.clone()) }
            })
            .collect(),
        Err(_) => vec![],
    }
}

/// Remove all HTML comment nodes (`<!-- … -->`) from the subtree rooted at
/// `node`.
pub fn remove_comment_nodes(node: &NodeRef) {
    let descendants: Vec<_> = node.descendants().collect();
    for n in descendants {
        if n.as_comment().is_some() {
            n.detach();
        }
    }
}

/// Undo the `<div data-readability-p-wrap>` wrappers that were inserted
/// earlier to group inline content.  A wrapper is removed (its children
/// are spliced in place) when it is the only element child of its parent
/// and the parent has no non-whitespace text of its own.
pub fn cleanup_readability_p_wrappers(root: &NodeRef) {
    let mut divs = vec![];
    if let Ok(iter) = root.select("div") {
        for d in iter {
            divs.push(d.as_node().clone());
        }
    }

    for div in divs.into_iter().rev() {
        if div.attr_value("data-readability-p-wrap").is_none() {
            continue;
        }

        let parent = match div.parent() {
            Some(p) => p,
            None => {
                if let Some(e) = div.as_element() {
                    e.attributes.borrow_mut().remove("data-readability-p-wrap");
                }
                continue;
            }
        };

        let mut parent_has_text = false;
        for child in parent.children() {
            if child.as_text().is_some() && !child.text_contents().trim().is_empty() {
                parent_has_text = true;
                break;
            }
        }

        let parent_elements = parent.element_children();
        let should_unwrap = !parent_has_text && parent_elements.len() == 1;

        if should_unwrap {
            let children: Vec<_> = div.children().collect();
            for child in children {
                child.detach();
                div.insert_before(child);
            }
            div.detach();
        } else if let Some(e) = div.as_element() {
            e.attributes.borrow_mut().remove("data-readability-p-wrap");
        }
    }
}

/// Returns true if the element has no meaningful content: no text, no images,
/// and the only child elements (if any) are <br> or <hr>.
pub fn is_element_without_content(node: &NodeRef) -> bool {
    if node.as_element().is_none() {
        return false;
    }
    if !node.text_contents().trim().is_empty() {
        return false;
    }
    if !select_descendants(node, "img").is_empty() {
        return false;
    }
    let children = node.element_children();
    if children.is_empty() {
        return true;
    }
    let brs = select_descendants(node, "br").len();
    let hrs = select_descendants(node, "hr").len();
    children.len() == brs + hrs
}

/// Flatten redundant single-child `<div>` / `<section>` wrappers.
///
/// Two cases are handled:
/// * The element is completely empty → it is removed.
/// * The element's only child is itself a `<div>` or `<section>` → the
///   wrapper's attributes are merged onto the child and the wrapper is
///   replaced by the child.
///
/// Nodes whose `id` starts with `"readability"` are left untouched so
/// that the algorithm's own marker elements survive.
pub fn simplify_nested_elements(article_content: &NodeRef) {
    let mut node = Some(article_content.clone());
    while let Some(current) = node {
        let mut next = get_next_node(&current, false);
        if current.parent().is_some() {
            if let Some(name) = current.element_name() {
                let name = name.to_lowercase();
                let id_is_readability = current
                    .attr_value("id")
                    .map(|id| id.starts_with("readability"))
                    .unwrap_or(false);
                if (name == "div" || name == "section") && !id_is_readability {
                    if is_element_without_content(&current) {
                        next = remove_and_get_next(&current);
                        node = next;
                        continue;
                    }
                    if contains_single_tag_in_element(&current, "div")
                        || contains_single_tag_in_element(&current, "section")
                    {
                        if let Some(child) = current.element_children().get(0).cloned() {
                            if let (Some(parent_e), Some(child_e)) =
                                (current.as_element(), child.as_element())
                            {
                                for (attr_name, attr) in
                                    parent_e.attributes.borrow().map.clone()
                                {
                                    child_e.attributes.borrow_mut().insert(
                                        attr_name.local.to_string(),
                                        attr.value.clone(),
                                    );
                                }
                            }
                            current.insert_before(child.clone());
                            current.detach();
                            node = Some(child);
                            continue;
                        }
                    }
                }
            }
        }
        node = next;
    }
}

/// Depth-first DOM iterator step.  Returns the next element node in a
/// depth-first traversal.
///
/// * `ignore_self_and_children = false` – descend into `node`'s children
///   first (normal DFS step).
/// * `ignore_self_and_children = true` – skip `node` and its subtree
///   entirely; useful when `node` is about to be detached.
///
/// Returns `None` when the end of the tree is reached.
pub fn get_next_node(node: &NodeRef, ignore_self_and_children: bool) -> Option<NodeRef> {
    // First check for kids if those aren't being ignored
    let first_child = node.first_element_child();
    if !ignore_self_and_children && first_child.is_some() {
        return first_child;
    }
    // Then for siblings...
    if let Some(next_sibling) = node.next_element_sibling() {
        return Some(next_sibling);
    }

    // And finally, move up the parent chain *and* find a sibling
    // (because this is depth-first traversal, we will have already
    // seen the parent nodes themselves).
    let mut current = node.parent();
    while let Some(p) = current {
        if let Some(sibling) = p.next_element_sibling() {
            return Some(sibling);
        }
        current = p.parent();
    }
    None
}

/// Detach `node` from the tree and return the next node in DFS order
/// (equivalent to `get_next_node(node, true)` followed by `node.detach()`).
pub fn remove_and_get_next(node: &NodeRef) -> Option<NodeRef> {
    let next = get_next_node(node, true);
    node.detach();
    next
}

/// Build the string used for class/id regex matching: the node's `class`
/// and `id` attributes joined by a space.
pub fn match_string_for_node(node: &NodeRef) -> String {
    concat_optionals(node.attr_value("class"), node.attr_value("id"), " ")
}

/// Walk the subtree rooted at `node` in DFS order and detach every
/// descendant for which `predicate(node, class_id_string)` returns `true`.
pub fn remove_matched_nodes<F>(node: &NodeRef, predicate: F)
where
    F: Fn(&NodeRef, &str) -> bool,
{
    let end_of_search_marker = get_next_node(node, true);
    let mut next = get_next_node(node, false);
    while next.is_some() && next != end_of_search_marker {
        let n = next.clone().unwrap();
        let match_str = match_string_for_node(&n);
        if predicate(&n, match_str.as_str()) {
            next = remove_and_get_next(&n);
        } else {
            next = get_next_node(&n, false);
        }
    }
}

/// Advance through the sibling list starting at `node` until an element
/// node or a non-whitespace text node is found.  Returns `None` at the end
/// of the sibling list.
pub fn next_element(node: Option<NodeRef>) -> Option<NodeRef> {
    let mut next = node;
    while let Some(ref n) = next {
        if n.as_element().is_some() || !n.text_contents().trim().is_empty() {
            break;
        }
        next = n.next_sibling();
    }
    next
}

/// Replace every descendant element matching `selector` with a copy that
/// has tag name `new_tag_name`, preserving attributes and children.
pub fn rename_tags_with_selector(node: &NodeRef, selector: &str, new_tag_name: &str) {
    for n in select_descendants(node, selector) {
        n.clone().clone_and_rename_element(new_tag_name);
    }
}

/// Return `true` if `node` is a whitespace-only text node or a `<br>`.
pub fn is_whitespace_node(node: &NodeRef) -> bool {
    if (node.as_text().is_some() && node.text_contents().trim().is_empty())
        || node.element_name() == Some("br")
    {
        return true;
    }

    false
}

/// Return `true` if `node` carries no meaningful content and can be safely
/// pruned.  Void elements, phrasing content, and nodes with an `id` or
/// `name` attribute are never considered empty (they may be referenced
/// elsewhere).
pub fn is_empty_node(node: &NodeRef, logger: &PerfLogger) -> bool {
    // self closing tags, aren't expected to have any children
    // and shouldn't be treated as empty
    if let Some(name) = node.element_name() {
        if SELF_CLOSING_TAGS.contains(name) {
            return false;
        }
    }

    // phrasing elements should remain even if they are empty
    if is_phrasing_content(node) {
        return false;
    }

    // if the node has an id or name then this node
    // can be potentially needed as a placeholder
    for attr_name in REFERENCING_ATTRIBUTES {
        if node.attr_value(attr_name).is_some() {
            return false;
        }
    }

    let txt = get_normalized_text_content(node, logger);

    txt.trim().is_empty() && select_descendants(node, "img").is_empty()
}

/// Return `true` if `node` is [phrasing content](https://html.spec.whatwg.org/multipage/dom.html#phrasing-content)
/// per the HTML spec.  Text nodes, elements in [`PHRASING_ELEMENTS`], and
/// `<a>` / `<del>` / `<ins>` whose *entire* child list is also phrasing
/// content all qualify.
pub fn is_phrasing_content(node: &NodeRef) -> bool {
    if node.as_text().is_some() {
        return true;
    }

    if let Some(name) = node.element_name() {
        if PHRASING_ELEMENTS.contains(name) {
            return true;
        }
    }
    if (node.element_name() == Some("a")
        || node.element_name() == Some("del")
        || node.element_name() == Some("ins"))
        && test_all_siblings(node.first_child(), is_phrasing_content)
    {
        return true;
    }
    false
}

/// Return `true` if every sibling starting at `node` (inclusive) satisfies
/// `test_func`.  An empty sibling list (i.e. `node = None`) vacuously
/// returns `true`.
pub fn test_all_siblings<F>(node: Option<NodeRef>, test_func: F) -> bool
where
    F: Fn(&NodeRef) -> bool,
{
    let mut next = node;
    while next.is_some() {
        let n = next.clone().unwrap();
        if !test_func(&n) {
            return false;
        }
        next = n.next_sibling();
    }
    true
}

/// Move every child node of `from` (in order) to be the last children of
/// `to`.  After the call, `from` has no children.
pub fn move_children(from: &NodeRef, to: &NodeRef) {
    let mut child = from.first_child();
    while child.is_some() {
        let child_unwraped = child.clone().unwrap();
        child = child_unwraped.next_sibling();
        to.append(child_unwraped);
    }
}

/// Return `true` if at least one descendant of `node` matching `sel`
/// satisfies `test_func`.
pub fn test_any_node_by_selector<F>(node: &NodeRef, sel: &str, test_func: F) -> bool
where
    F: Fn(&NodeRef) -> bool,
{
    for n in select_descendants(node, sel) {
        if test_func(&n) {
            return true;
        }
    }
    false
}

/// Returns true if the passed node contains only single
/// node that matches the tag_name, false otherwise
pub fn contains_single_tag_in_element(node: &NodeRef, tag_name: &str) -> bool {
    let mut elements = vec![];
    let mut non_elements = vec![];
    for c in node.children() {
        if c.as_element().is_some() {
            elements.push(c);
        } else {
            non_elements.push(c);
        }
    }
    // There should be exactly 1 element child with given tag
    if elements.len() != 1 || elements.get(0).unwrap().element_name() != Some(tag_name) {
        return false;
    }

    // And there should be no text nodes with real content
    for c in non_elements {
        if HAS_CONTENT.is_match(c.text_contents().as_str()) {
            return false;
        }
    }

    true
}

/// Return `true` if any ancestor of `node` (up to `max_depth` levels)
/// has tag name `ancestor_tag_name`.  Pass `0` for `max_depth` to search
/// all the way to the root.
pub fn has_ancestor_tag(node: &NodeRef, ancestor_tag_name: &str, max_depth: i16) -> bool {
    has_ancestor_tag_with_predicate(node, ancestor_tag_name, max_depth, |_| true)
}

/// Like [`has_ancestor_tag`], but the matching ancestor must also satisfy
/// `predicate`.  Useful for checking properties on the ancestor (e.g.
/// whether a `<table>` ancestor has been classified as a data table).
pub fn has_ancestor_tag_with_predicate<F>(
    node: &NodeRef,
    ancestor_tag_name: &str,
    max_depth: i16,
    predicate: F,
) -> bool
where
    F: Fn(&NodeRef) -> bool,
{
    let mut depth = 0;
    let mut node = node.clone();
    while let Some(p) = node.parent() {
        depth += 1;
        if max_depth > 0 && depth > max_depth {
            return false;
        }
        if p.element_name() == Some(ancestor_tag_name) && predicate(&p) {
            return true;
        }
        node = p;
    }
    false
}

/// Collect all descendants of `container` matching *any* of the given
/// CSS selectors into a single flat `Vec`.
pub fn concate_nodes_with_selectors(container: &NodeRef, selectors: Vec<&str>) -> Vec<NodeRef> {
    let mut res = vec![];
    for s in selectors {
        res.extend(select_descendants(container, s));
    }
    res
}

/// Walk up the parent chain from `node` and collect ancestors into a `Vec`
/// (nearest ancestor first).  Stop after `max_depth` levels; pass `0` to
/// collect all ancestors up to the root.
pub fn get_node_ancestors(node: &NodeRef, max_depth: i16) -> Vec<NodeRef> {
    let mut ancestors = vec![];
    let mut depth = 1;
    let mut current = node.parent();
    while let Some(p) = current {
        ancestors.push(p.clone());
        if max_depth > 0 && depth == max_depth {
            break;
        }
        depth += 1;
        current = p.parent();
    }
    ancestors
}

/// Compute the ratio of link-text length to total text length inside `node`.
/// Anchor links whose `href` is a bare hash (`#…`) contribute at only 30 %
/// of their actual length, because in-page navigation links are common in
/// legitimate content.  Returns `0.0` when the node has no text at all.
pub fn get_link_density(node: &NodeRef, logger: &PerfLogger) -> f64 {
    start_span!(logger, GET_LINK_DENSITY);
    add_point_to_span_str!(logger, GET_LINK_DENSITY, "get_node_normalized_text_begin");
    let text_length = get_normalized_text_length(node, logger);
    add_point_to_span_str!(logger, GET_LINK_DENSITY, "get_node_normalized_text_end");
    if text_length == 0 {
        annotate_span_str!(
            logger,
            GET_LINK_DENSITY,
            "early return because node content is empty"
        );
        end_span!(logger, GET_LINK_DENSITY);
        return 0.0;
    }

    let mut link_length = 0.0_f64;
    add_point_to_span_str!(logger, GET_LINK_DENSITY, "sum_link_text_lengths_begin");
    for a in select_descendants(node, "a") {
        let mut coefficient = 1.0;
        if let Some(href) = a.attr_value("href") {
            if href.trim().starts_with('#') {
                coefficient = 0.3;
            }
        }
        link_length += get_normalized_text_length(&a, logger) as f64 * coefficient;
    }
    add_point_to_span_str!(logger, GET_LINK_DENSITY, "sum_link_text_lengths_end");
    let result = link_length / (text_length as f64);
    end_span!(logger, GET_LINK_DENSITY);
    result
}

fn get_normalized_text_length(node: &NodeRef, logger: &PerfLogger) -> usize {
    start_span!(logger, NORMALIZE_AND_COUNT_CHARS);
    add_point_to_span_str!(
        logger,
        NORMALIZE_AND_COUNT_CHARS,
        "get_normalized_txt_begin"
    );
    let txt = get_normalized_text_content(node, logger);
    add_point_to_span_str!(logger, NORMALIZE_AND_COUNT_CHARS, "get_normalized_txt_end");

    add_point_to_span_str!(logger, NORMALIZE_AND_COUNT_CHARS, "count_chars_begin");
    let count = txt.chars().count();
    add_point_to_span_str!(logger, NORMALIZE_AND_COUNT_CHARS, "count_chars_end");
    end_span!(logger, NORMALIZE_AND_COUNT_CHARS);
    count
}

/// Extract the full text content of `node` and collapse all runs of
/// whitespace into single spaces (leading/trailing whitespace is trimmed).
pub fn get_normalized_text_content(node: &NodeRef, logger: &PerfLogger) -> String {
    start_span!(logger, NORMALIZE_NODE_TEXT);

    add_point_to_span_str!(logger, NORMALIZE_NODE_TEXT, "get_text_contents_begin");
    let txt = node.text_contents();
    add_point_to_span_str!(logger, NORMALIZE_NODE_TEXT, "get_text_contents_end");
    add_point_to_span_str!(logger, NORMALIZE_NODE_TEXT, "remove_duplicate_spaces_begin");
    let txt = normalize_text(txt.trim());
    add_point_to_span_str!(logger, NORMALIZE_NODE_TEXT, "remove_duplicate_spaces_end");
    end_span!(logger, NORMALIZE_NODE_TEXT);
    txt
}

static NORMALIZE_REGEX: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"\s{2,}").unwrap());

/// Collapse every run of two or more whitespace characters in `src` into a
/// single ASCII space.
pub fn normalize_text(src: &str) -> String {
    NORMALIZE_REGEX.replace_all(src, " ").to_string()
}

/// Return `true` if `text` (after trimming and zero-width-space removal)
/// is a known ad or "loading …" placeholder in any supported language
/// (English, French, Spanish, German, Chinese, Russian).
pub fn matches_ad_or_loading(text: &str) -> bool {
    fn is_loading_word(s: &str) -> bool {
        let lowered = s.to_lowercase();
        let bases = [
            "loading",
            "正在加载",
            "загрузка",
            "chargement",
            "cargando",
        ];
        for base in bases {
            if lowered == base {
                return true;
            }
            let dots = format!("{}...", base);
            if lowered == dots {
                return true;
            }
            let ellipsis = format!("{}…", base);
            if lowered == ellipsis {
                return true;
            }
        }
        false
    }

    fn is_ad_word(s: &str) -> bool {
        let lowered = s.to_lowercase();
        matches!(
            lowered.as_str(),
            "ad"
                | "advertising"
                | "advertisement"
                | "pub"
                | "publicite"
                | "publicité"
                | "werb"
                | "werbung"
                | "广告"
                | "реклама"
                | "anuncio"
        )
    }

    let trimmed = text.trim();
    if is_ad_word(trimmed) || is_loading_word(trimmed) {
        return true;
    }

    let compact: String = trimmed
        .chars()
        .filter(|c| !c.is_whitespace() && *c != '\u{200b}' && *c != '\u{feff}')
        .collect();
    is_ad_word(compact.as_str()) || is_loading_word(compact.as_str())
}

/// Like [`normalize_text`], but preserves Unicode non-breaking and
/// typographic space characters (U+00A0, U+2000–U+200A, etc.) instead
/// of collapsing them.  Used when generating the final output so that
/// intentional non-breaking spaces survive.
pub fn normalize_text_preserve_nbsp(src: &str) -> String {
    let mut out = String::new();
    let mut in_ws = false;
    for ch in src.chars() {
        if is_preserved_unicode_space(ch) {
            out.push(ch);
            in_ws = false;
            continue;
        }
        if is_ascii_whitespace(ch) {
            if !in_ws {
                out.push(' ');
                in_ws = true;
            }
        } else {
            out.push(ch);
            in_ws = false;
        }
    }
    out
}

fn is_ascii_whitespace(ch: char) -> bool {
    matches!(ch, ' ' | '\t' | '\n' | '\r' | '\x0C')
}

fn is_preserved_unicode_space(ch: char) -> bool {
    matches!(ch,
        '\u{00A0}' | // NO-BREAK SPACE
        '\u{1680}' | // OGHAM SPACE MARK
        '\u{2000}' | '\u{2001}' | '\u{2002}' | '\u{2003}' | '\u{2004}' |
        '\u{2005}' | '\u{2006}' | '\u{2007}' | '\u{2008}' | '\u{2009}' |
        '\u{200A}' | // EN/EM/THIN/HAIR spaces
        '\u{202F}' | // NARROW NO-BREAK SPACE
        '\u{205F}' | // MEDIUM MATHEMATICAL SPACE
        '\u{3000}'   // IDEOGRAPHIC SPACE
    )
}

/// Compute a similarity score between two strings in the range `[0.0, 1.0]`.
///
/// The algorithm tokenises both strings on non-alphanumeric boundaries,
/// lowercases, then measures how much of `text_b`'s token sequence is
/// *not* present in `text_a`.  A score of `1.0` means `text_b`'s tokens
/// are a subset of `text_a`'s; `0.0` means no overlap.  Used to detect
/// when an `<h1>` / `<h2>` duplicates the page title.
pub fn text_similarity(text_a: &str, text_b: &str) -> f64 {
    let tokens_a: HashSet<String> = TOKENIZE_REGEX
        .split(&text_a.to_lowercase())
        .filter(|s| !s.is_empty())
        .map(|s| s.to_string())
        .collect();
    let tokens_b: Vec<String> = TOKENIZE_REGEX
        .split(&text_b.to_lowercase())
        .filter(|s| !s.is_empty())
        .map(|s| s.to_string())
        .collect();

    if tokens_a.is_empty() || tokens_b.is_empty() {
        return 0.0;
    }

    let uniq_tokens_b: Vec<&String> = tokens_b.iter().filter(|t| !tokens_a.contains(*t)).collect();
    let uniq_len: usize = uniq_tokens_b.iter().map(|s| s.len()).sum::<usize>()
        + uniq_tokens_b.len().saturating_sub(1);
    let total_len: usize =
        tokens_b.iter().map(|s| s.len()).sum::<usize>() + tokens_b.len().saturating_sub(1);
    let distance_b = uniq_len as f64 / total_len as f64;
    1.0 - distance_b
}

/// Decode the five named HTML entities (`&quot;` `&amp;` `&apos;` `&lt;`
/// `&gt;`) and all numeric character references (`&#…;` / `&#x…;`) in
/// `value`.  Invalid code points are replaced with U+FFFD.
pub fn unescape_html_entities(value: &str) -> String {
    if value.is_empty() {
        return value.to_string();
    }

    let replaced = UNESCAPE_NAMED_ENTITIES.replace_all(value, |caps: &regex::Captures| {
        match caps.get(0).map(|m| m.as_str()).unwrap_or_default() {
            "&quot;" => "\"",
            "&amp;" => "&",
            "&apos;" => "'",
            "&lt;" => "<",
            "&gt;" => ">",
            _ => "",
        }
    });

    let replaced = UNESCAPE_NUMERIC_ENTITIES.replace_all(&replaced, |caps: &regex::Captures| {
        let num = if let Some(hex) = caps.get(1) {
            u32::from_str_radix(hex.as_str(), 16).unwrap_or(0)
        } else if let Some(dec) = caps.get(2) {
            dec.as_str().parse::<u32>().unwrap_or(0)
        } else {
            0
        };

        let num = if num == 0 || num > 0x10FFFF || (0xD800..=0xDFFF).contains(&num) {
            0xFFFD
        } else {
            num
        };
        std::char::from_u32(num).unwrap_or('\u{FFFD}').to_string()
    });

    replaced.into_owned()
}

/// Normalise whitespace in every text node under `root`, merging adjacent
/// text nodes and collapsing runs of whitespace while preserving
/// non-breaking spaces.  Content inside `<pre>`, `<code>`, `<textarea>`,
/// `<script>`, `<style>`, `<svg>`, and `<math>` is left untouched.
pub fn normalize_text_nodes(root: &NodeRef) {
    let skip_tags = [
        "pre",
        "code",
        "textarea",
        "script",
        "style",
        "svg",
        "math",
    ];
    let nodes: Vec<_> = root.descendants().collect();
    for n in nodes {
        if let Some(text) = n.as_text() {
            if let Some(parent) = n.parent() {
                if let Some(tag) = parent.element_name() {
                    let tag = tag.to_lowercase();
                    if skip_tags.contains(&tag.as_str()) {
                        continue;
                    }
                }
            }
            let current = text.borrow().to_string();
            if let Some(next) = n.next_sibling() {
                if let Some(next_text) = next.as_text() {
                    let merged = format!("{}{}", current, next_text.borrow());
                    let normalized = normalize_text_preserve_nbsp(merged.as_str());
                    let new_node = NodeRef::new_text(normalized);
                    n.insert_after(new_node);
                    n.detach();
                    next.detach();
                    continue;
                }
            }
            let normalized = normalize_text_preserve_nbsp(current.as_str());
            if normalized.trim().is_empty() {
                let prev = n.previous_sibling();
                let next = n.next_sibling();
                match (prev.clone(), next.clone()) {
                    (Some(prev_node), Some(next_node)) => {
                        if let Some(prev_text) = prev_node.as_text() {
                            let mut prev_val = prev_text.borrow().to_string();
                            if let Some(last) = prev_val.chars().last() {
                                if is_phrasing_content(&prev_node)
                                    && !is_ascii_whitespace(last)
                                    && !is_preserved_unicode_space(last)
                                {
                                    prev_val.push(' ');
                                    let new_prev =
                                        NodeRef::new_text(normalize_text_preserve_nbsp(
                                            prev_val.as_str(),
                                        ));
                                    prev_node.insert_before(new_prev);
                                    prev_node.detach();
                                }
                            }
                        }
                        if let Some(next_text) = next_node.as_text() {
                            let mut next_val = next_text.borrow().to_string();
                            if let Some(first) = next_val.chars().next() {
                                if is_phrasing_content(&next_node)
                                    && !is_ascii_whitespace(first)
                                    && !is_preserved_unicode_space(first)
                                {
                                    next_val.insert(0, ' ');
                                    let new_next =
                                        NodeRef::new_text(normalize_text_preserve_nbsp(
                                            next_val.as_str(),
                                        ));
                                    next_node.insert_before(new_next);
                                    next_node.detach();
                                }
                            }
                        }
                    }
                    (None, Some(next_node)) => {
                        if is_phrasing_content(&next_node) {
                            if let Some(next_text) = next_node.as_text() {
                                let mut next_val = next_text.borrow().to_string();
                                if let Some(first) = next_val.chars().next() {
                                    if !is_ascii_whitespace(first)
                                        && !is_preserved_unicode_space(first)
                                    {
                                        next_val.insert(0, ' ');
                                        let new_next = NodeRef::new_text(
                                            normalize_text_preserve_nbsp(next_val.as_str()),
                                        );
                                        next_node.insert_before(new_next);
                                        next_node.detach();
                                    }
                                }
                            } else {
                                next_node.insert_before(NodeRef::new_text(" "));
                            }
                        }
                    }
                    (Some(prev_node), None) => {
                        if is_phrasing_content(&prev_node) {
                            if let Some(prev_text) = prev_node.as_text() {
                                let mut prev_val = prev_text.borrow().to_string();
                                if let Some(last) = prev_val.chars().last() {
                                    if !is_ascii_whitespace(last)
                                        && !is_preserved_unicode_space(last)
                                    {
                                        prev_val.push(' ');
                                        let new_prev = NodeRef::new_text(
                                            normalize_text_preserve_nbsp(prev_val.as_str()),
                                        );
                                        prev_node.insert_before(new_prev);
                                        prev_node.detach();
                                    }
                                }
                            } else {
                                prev_node.insert_after(NodeRef::new_text(" "));
                            }
                        }
                    }
                    _ => {}
                }
                n.detach();
                continue;
            }
            let normalized = normalized;
            if normalized != current {
                let new_node = NodeRef::new_text(normalized);
                n.insert_after(new_node);
                n.detach();
            }
        }
    }
}

fn get_class_and_id_attr_weight(attr_value: &str) -> i64 {
    let mut weight = 0;
    if NEGATIVE_CLASSES_AND_IDS.is_match(attr_value) {
        weight -= 25;
    }

    if POSITIVE_CLASSES_AND_IDS.is_match(attr_value) {
        weight += 25;
    }

    weight
}

/// Score a node's `class` and `id` attributes against the positive and
/// negative word-lists.  Each attribute contributes +25 (positive match)
/// or −25 (negative match); the two are summed.
pub fn get_class_and_id_weight(node: &NodeRef) -> i64 {
    let mut weight = 0;
    if let Some(class_name) = node.attr_value("class") {
        weight += get_class_and_id_attr_weight(class_name.as_str())
    }

    if let Some(tag_id) = node.attr_value("id") {
        weight += get_class_and_id_attr_weight(tag_id.as_str())
    }

    weight
}

/// Returns true if the passed node has at least one child with name matches
/// any of the names in the look_up_tag_names
pub fn node_contains_any_tag_of(node: &NodeRef, look_up_tag_names: &[&str]) -> bool {
    for tag in look_up_tag_names {
        if !select_descendants(node, tag).is_empty() {
            return true;
        }
    }
    false
}

/// convert images and figures that have properties like data-src into images that can be loaded without JS
pub fn fix_lazy_images(node: &NodeRef) {
    apply(node, &["img", "picture", "figure"], |n, tag_name| {
        let src = n.attr_value("src");
        if let Some(src_val) = src.clone() {
            if let Some(caps) = B64_DATA_URL.captures(src_val.as_str()) {
                let mime = caps.get(1).map(|m| m.as_str()).unwrap_or_default();
                if mime != "image/svg+xml" {
                    let mut src_could_be_removed = false;
                    if let Some(e) = n.as_element() {
                        for (name, attr) in e.attributes.borrow().clone().map {
                            if name.local.to_string() == "src" {
                                continue;
                            }
                            if IMAGE_EXTENSION.is_match(attr.value.as_str()) {
                                src_could_be_removed = true;
                                break;
                            }
                        }
                    }
                    if src_could_be_removed {
                        let b64starts = caps.get(0).map(|m| m.as_str().len()).unwrap_or(0);
                        let b64length = src_val.len().saturating_sub(b64starts);
                        if b64length < 133 {
                            if let Some(e) = n.as_element() {
                                e.attributes.borrow_mut().remove("src");
                            }
                        }
                    }
                }
            }
        }

        let src = n.attr_value("src");
        let srcset = n.attr_value("srcset");
        let class_name = n.attr_value("class").unwrap_or_default().to_lowercase();

        if (src.is_some() || (srcset.is_some() && srcset.as_deref() != Some("null")))
            && !class_name.contains("lazy")
        {
            return;
        }

        if let Some(e) = n.as_element() {
            let mut tmp: HashMap<String, String> = HashMap::new();
            for (name, attr) in e.attributes.borrow().clone().map {
                let local_name = name.local.to_string();
                if local_name == "src" || local_name == "srcset" || local_name == "alt" {
                    continue;
                }

                let copy_to_attr: Option<&str> = if SRCSET_EXTENSION.is_match(attr.value.as_str()) {
                    Some("srcset")
                } else if SRC_EXTENSION.is_match(attr.value.as_str()) {
                    Some("src")
                } else {
                    None
                };

                if let Some(copy_to_attr) = copy_to_attr {
                    if tag_name == "img" || tag_name == "picture" {
                        tmp.insert(copy_to_attr.to_string(), attr.value.clone());
                    } else if tag_name == "figure"
                        && n.select_first("img").is_err()
                        && n.select_first("picture").is_err()
                    {
                        let img = new_html_element("img");
                        img.as_element()
                            .unwrap()
                            .attributes
                            .borrow_mut()
                            .insert(copy_to_attr, attr.value.clone());
                        n.append(img);
                    }
                }
            }

            for (name, value) in tmp {
                e.attributes.borrow_mut().insert(name, value);
            }
        }
    });
}

fn is_single_image(node: &NodeRef) -> bool {
    let mut current = node.clone();
    loop {
        if current.element_name() == Some("img") {
            return true;
        }
        let children = current.element_children();
        if children.len() != 1 || !current.text_contents().trim().is_empty() {
            return false;
        }
        current = children[0].clone();
    }
}

/// Replace JS-dependent image placeholders with their `<noscript>` fallbacks.
///
/// Two passes are performed:
/// 1. Any `<img>` that has no recognised image attribute (`src`, `srcset`,
///    `data-src`, `data-srcset`, or an attribute whose value looks like an
///    image URL) is removed entirely.
/// 2. Each `<noscript>` that contains exactly one image is parsed; if the
///    preceding sibling is also a single-image wrapper the placeholder is
///    replaced with the noscript image, inheriting any `src`/`srcset` from
///    the old placeholder under a `data-old-*` key.
pub fn unwrap_noscript_images(doc: &NodeRef) {
    let imgs = select_descendants(doc, "img");
    for img in imgs {
        if let Some(e) = img.as_element() {
            let mut has_image_attr = false;
            for (name, attr) in e.attributes.borrow().clone().map {
                let local = name.local.to_string();
                match local.as_str() {
                    "src" | "srcset" | "data-src" | "data-srcset" => {
                        has_image_attr = true;
                        break;
                    }
                    _ => {
                        if IMAGE_EXTENSION.is_match(attr.value.as_str()) {
                            has_image_attr = true;
                            break;
                        }
                    }
                }
            }
            if !has_image_attr {
                img.detach();
            }
        }
    }

    let noscripts = select_descendants(doc, "noscript");
    for noscript in noscripts {
        if !is_single_image(&noscript) {
            continue;
        }
        let inner = noscript.inner_html();
        let tmp = parse_html(format!("<div>{}</div>", inner).as_str());
        let new_img = tmp.select_first("img").ok().map(|n| n.as_node().clone());
        if new_img.is_none() {
            continue;
        }

        if let Some(prev_element) = noscript.previous_element_sibling() {
            if is_single_image(&prev_element) {
                let mut prev_img = prev_element.clone();
                if prev_img.element_name() != Some("img") {
                    if let Ok(img) = prev_element.select_first("img") {
                        prev_img = img.as_node().clone();
                    }
                }

                if let (Some(prev_e), Some(new_e)) =
                    (prev_img.as_element(), new_img.clone().unwrap().as_element())
                {
                    for (name, attr) in prev_e.attributes.borrow().clone().map {
                        if attr.value.is_empty() {
                            continue;
                        }
                        let local = name.local.to_string();
                        let should_copy = local == "src"
                            || local == "srcset"
                            || IMAGE_EXTENSION.is_match(attr.value.as_str());
                        if !should_copy {
                            continue;
                        }
                        if let Some(existing) = new_e.attributes.borrow().get(local.as_str()) {
                            if existing == attr.value.as_str() {
                                continue;
                            }
                        }
                        let mut attr_name = local.clone();
                        if new_e.attributes.borrow().contains(local.as_str()) {
                            attr_name = format!("data-old-{}", local);
                        }
                        new_e
                            .attributes
                            .borrow_mut()
                            .insert(attr_name.as_str(), attr.value.clone());
                    }
                }

                let new_img_node = new_img.unwrap();
                prev_element.insert_after(new_img_node);
                prev_element.detach();
            }
        }
    }
}

/// Return `true` if `node` (an `<embed>`, `<object>`, or `<iframe>`) looks
/// like an embedded video from a known provider (YouTube, Vimeo, Dailymotion,
/// etc.).  Such embeds are preserved even though they would otherwise be
/// stripped as non-content.
pub fn is_possibly_useful_video_node(node: &NodeRef, tag: &str) -> bool {
    if tag != "embed" && tag != "object" && tag != "iframe" {
        return false;
    }

    if let Some(e) = node.as_element() {
        // If this embed has attribute that matches video regex, don't delete it.
        for (_, attr) in e.attributes.borrow().clone().map {
            if VIDEO_ATTRS_REGEX.is_match(attr.value.as_str()) {
                return true;
            }
        }

        // For embed with <object> tag, check inner HTML as well.
        if node.element_name() == Some("object")
            && VIDEO_ATTRS_REGEX.is_match(node.inner_html().as_str())
        {
            return true;
        }
    }

    false
}

/// If `node` is an unlikely content candidate (its role or class/id string
/// matches the unlikely regex and does *not* also match the "maybe a
/// candidate" exception list), detach it and return the next DFS node.
/// Otherwise return `None` (the node is kept).
pub fn strip_unlikely_and_get_next(node: &NodeRef, matching_str: &str) -> Option<NodeRef> {
    if let Some(role) = node.attr_value("role") {
        if UNLIKELY_ROLES.contains(role.as_str()) {
            return remove_and_get_next(node);
        }
    }
    if UNLIKELY_CANDIDATES_REGEX.is_match(matching_str)
        && !MAYBE_A_CANDIDATE.is_match(matching_str)
        && !has_ancestor_tag(node, "table", DEFAULT_MAX_ANCESTORS_LOOKUP_DEPTH)
        && !has_ancestor_tag(node, "code", DEFAULT_MAX_ANCESTORS_LOOKUP_DEPTH)
        && node.element_name() != Some("body")
        && node.element_name() != Some("a")
    {
        // Remove unlikely candidate
        return remove_and_get_next(node);
    }
    None
}

/// Detach every descendant of `container` with tag `tag_name` for which
/// `condition(node, tag_name)` returns `true`.  Iteration is done in
/// reverse document order so that detaching a node does not invalidate
/// the remaining iterator.
pub fn remove_nodes<F>(container: &NodeRef, tag_name: &str, condition: F)
where
    F: Fn(&NodeRef, &str) -> bool,
{
    for node in select_descendants(container, tag_name).into_iter().rev() {
        if condition(&node, tag_name) {
            node.detach();
        }
    }
}

/// Count the number of whitespace-delimited tokens in `text`.
///
/// # Examples
///
/// ```rust
/// use readable_rs::shared_utils::word_count;
///
/// assert_eq!(word_count("Hello World      Another word"), 4);
/// assert_eq!(word_count(""), 0);
/// assert_eq!(word_count("   "), 0);
/// ```
pub fn word_count(text: &str) -> usize {
    text.split_whitespace().count()
}

/// Apply `func(node, selector)` to every descendant of `root_node` that
/// matches any selector in `selectors`.  Each selector's matches are
/// visited in reverse document order (safe for detach).  Invalid CSS
/// selectors are silently skipped.
///
/// # Examples
///
/// ```rust
/// use std::cell::Cell;
/// use readable_rs::parser::parse_html;
/// use readable_rs::shared_utils::apply;
///
/// let doc = parse_html("<div><p>one</p><p>two</p></div>");
/// let count = Cell::new(0usize);
/// apply(&doc, &["p"], |_node, _sel| { count.set(count.get() + 1); });
/// assert_eq!(count.get(), 2);
/// ```
pub fn apply<F>(root_node: &NodeRef, selectors: &[&str], func: F)
where
    F: Fn(&NodeRef, &str),
{
    for s in selectors {
        for n in select_descendants(root_node, s).into_iter().rev() {
            func(&n, s);
        }
    }
}

/// Resolve a `<base href>` value against the document URI.  If
/// `base_path` is empty the document URI is returned unchanged.
pub fn resolve_base_uri(doc_uri: &str, base_path: &str) -> String {
    if base_path.is_empty() {
        return doc_uri.to_string();
    }
    if let Ok(parsed_url) = url::Url::parse(doc_uri) {
        if let Ok(base) = parsed_url.join(base_path) {
            return base.to_string();
        }
    }
    base_path.to_string()
}

/// Convert a potentially-relative URI to an absolute one using the
/// document URI and an optional `<base href>` path.  Bare hash links
/// (`#…`) are left as-is when they resolve to the same document.
pub fn to_absolute_uri(uri: &str, doc_uri: &str, base_path: &str) -> String {
    // Leave hash links alone if the base_path is empty
    // or if it not a relative uri
    let uri = uri.trim();
    if let Ok(parsed) = url::Url::parse(uri) {
        return parsed.into();
    }
    if base_path.is_empty() && uri.starts_with('#') {
        return String::from(uri);
    }
    let base_uri = resolve_base_uri(doc_uri, base_path);
    if base_uri == doc_uri && uri.starts_with('#') {
        return String::from(uri);
    }

    if let Ok(parsed_url) = url::Url::parse(base_uri.as_str()) {
        if let Ok(parsed_url) = parsed_url.join(uri) {
            return parsed_url.into();
        }
    }

    uri.to_string()
}

/// Resolve each URL in a `srcset` attribute against the document base.
fn resolve_srcset(srcset: &str, doc_uri: &str, base_path: &str) -> String {
    let mut out = String::new();
    for caps in SRCSET_URL.captures_iter(srcset) {
        let url = caps.get(1).map(|m| m.as_str()).unwrap_or_default();
        let descriptor = caps.get(2).map(|m| m.as_str()).unwrap_or("");
        let trailing = caps.get(3).map(|m| m.as_str()).unwrap_or("");
        let absolute = to_absolute_uri(url, doc_uri, base_path);
        out.push_str(absolute.as_str());
        out.push_str(descriptor);
        out.push_str(trailing);
    }
    if out.is_empty() {
        srcset.to_string()
    } else {
        out
    }
}

/// Rewrite all relative URLs in `<a>`, `<img>`, `<picture>`, `<figure>`,
/// `<video>`, `<audio>`, and `<source>` elements under `node` to absolute
/// URLs using `doc_uri` and the optional `<base href>` path.
///
/// Special handling for `<a>` links:
/// * `javascript:` hrefs → the `<a>` is replaced by a `<span>` (attributes
///   like `id` / `name` are preserved if present).
/// * Hash links that point to the element's own `id` → same replacement,
///   because the anchor target is preserved as a `<span id>`.
/// * All other hrefs (relative or absolute) → resolved normally.
pub fn replace_relative_urls_with_absolute(node: &NodeRef, doc_uri: &str, base_path: &str) {
    for link in select_descendants(node, "a") {
        if let Some(href) = link.attr_value("href") {
            let replace_link = |link: &NodeRef, preserve_attrs: bool| {
                let mut child_count = 0usize;
                let mut single_text_child = false;
                for child in link.children() {
                    child_count += 1;
                    if child_count == 1 && child.as_text().is_some() {
                        single_text_child = true;
                    } else {
                        single_text_child = false;
                    }
                }

                if !preserve_attrs && child_count == 1 && single_text_child {
                    let text_node = NodeRef::new_text(link.text_contents());
                    link.insert_before(text_node);
                    link.detach();
                    return;
                }

                let container = new_html_element("span");
                if preserve_attrs {
                    if let (Some(src), Some(dst)) = (link.as_element(), container.as_element()) {
                        for (attr_name, attr) in src.attributes.borrow().map.clone() {
                            if attr_name.local.to_string() == "href" {
                                continue;
                            }
                            dst.attributes
                                .borrow_mut()
                                .insert(attr_name.local.to_string(), attr.value.clone());
                        }
                    }
                }
                while let Some(child) = link.first_child() {
                    container.append(child);
                }
                link.insert_before(container);
                link.detach();
            };

            if href.starts_with("javascript:") {
                // Match Readability.js behavior for javascript: links.
                let preserve_attrs =
                    link.attr_value("id").is_some() || link.attr_value("name").is_some();
                replace_link(&link, preserve_attrs);
                continue;
            }

            if href.starts_with('#') {
                if let Some(id) = link.attr_value("id") {
                    if href == format!("#{}", id) {
                        replace_link(&link, true);
                        continue;
                    }
                }
                let absolute = to_absolute_uri(href.as_str(), doc_uri, base_path);
                if let Some(e) = link.as_element() {
                    e.attributes.borrow_mut().insert("href", absolute);
                }
                continue;
            }
            let absolute = to_absolute_uri(href.as_str(), doc_uri, base_path);
            if let Some(e) = link.as_element() {
                e.attributes.borrow_mut().insert("href", absolute);
            }
        }
    }

    for tag in ["img", "picture", "figure", "video", "audio", "source"] {
        for media in select_descendants(node, tag) {
            if let Some(src) = media.attr_value("src") {
                let absolute = to_absolute_uri(src.as_str(), doc_uri, base_path);
                if let Some(e) = media.as_element() {
                    e.attributes.borrow_mut().insert("src", absolute);
                }
            }
            if let Some(poster) = media.attr_value("poster") {
                let absolute = to_absolute_uri(poster.as_str(), doc_uri, base_path);
                if let Some(e) = media.as_element() {
                    e.attributes.borrow_mut().insert("poster", absolute);
                }
            }
            if let Some(srcset) = media.attr_value("srcset") {
                let absolute = resolve_srcset(srcset.as_str(), doc_uri, base_path);
                if let Some(e) = media.as_element() {
                    e.attributes.borrow_mut().insert("srcset", absolute);
                }
            }
        }
    }
}

/// Shared test helper: count elements matching a CSS selector in a document.
#[cfg(test)]
pub(crate) fn count_elements(doc: &NodeRef, tag_name: &str) -> usize {
    doc.select(tag_name).unwrap().count()
}

#[cfg(test)]
mod tests {
    use crate::parser::parse_html;
    use crate::utils::*;

    // --- 4c: B64_DATA_URL and SRCSET_URL regex correctness ---

    #[test]
    fn b64_data_url_matches_valid_data_uri() {
        // A well-formed data: URI with whitespace around the mime / base64 parts
        let input = "data: image/png ; base64 , iVBORw0KGgo=";
        assert!(
            B64_DATA_URL.is_match(input),
            "B64_DATA_URL should match a valid data URI; got no match on: {input:?}"
        );
        // Capture group 1 should be the mime type
        let caps = B64_DATA_URL.captures(input).unwrap();
        assert_eq!(caps.get(1).unwrap().as_str(), "image/png");
    }

    #[test]
    fn b64_data_url_does_not_match_non_data_uri() {
        assert!(!B64_DATA_URL.is_match("https://example.com/img.png"));
    }

    #[test]
    fn srcset_url_parses_single_entry() {
        let input = "https://example.com/img.png 2x";
        let caps: Vec<_> = SRCSET_URL.captures_iter(input).collect();
        assert!(!caps.is_empty(), "SRCSET_URL should match a srcset entry");
        assert_eq!(caps[0].get(1).unwrap().as_str(), "https://example.com/img.png");
        assert_eq!(caps[0].get(2).unwrap().as_str().trim(), "2x");
    }

    #[test]
    fn srcset_url_parses_multiple_entries() {
        let input = "small.jpg 480w, large.jpg 800w";
        let caps: Vec<_> = SRCSET_URL.captures_iter(input).collect();
        assert_eq!(caps.len(), 2, "SRCSET_URL should match both srcset entries");
        assert_eq!(caps[0].get(1).unwrap().as_str(), "small.jpg");
        assert_eq!(caps[1].get(1).unwrap().as_str(), "large.jpg");
    }

    // --- 4d: has_ancestor_tag_with_predicate depth check ---

    #[test]
    fn has_ancestor_tag_with_depth_3_finds_at_exactly_3() {
        // Structure: <div><section><span><p>leaf</p></span></section></div>
        // From <p>, "div" is at depth 3 (section=1, span=2 … wait, let's just be precise:
        //   parent chain of <p>: span (depth 1), section (depth 2), div (depth 3)
        let doc = parse_html("<div><section><span><p>leaf</p></span></section></div>");
        let p = doc.select_first("p").unwrap().as_node().clone();
        // depth limit = 3 should find "div"
        assert!(
            has_ancestor_tag(&p, "div", 3),
            "should find 'div' ancestor at depth 3"
        );
        // depth limit = 2 should NOT find "div"
        assert!(
            !has_ancestor_tag(&p, "div", 2),
            "should NOT find 'div' ancestor when max_depth=2"
        );
    }

    #[test]
    fn test_negative_classes_weight() {
        let negatives = "hidden|banner|combx|comment|com-|contact|footer|gdpr|masthead|media|meta|outbrain|promo|related|scroll|share|shoutbox|sidebar|skyscraper|sponsor|shopping|tags|widget".split('|').collect::<Vec<_>>();
        for n in negatives {
            let attr_value = format!("some random value {}", n);
            assert_eq!(get_class_and_id_attr_weight(attr_value.as_str()), -25);
            let attr_value = attr_value.to_uppercase();
            assert_eq!(get_class_and_id_attr_weight(attr_value.as_str()), -25);
        }

        assert_eq!(get_class_and_id_attr_weight("hid"), -25);
        assert_eq!(get_class_and_id_attr_weight("aaassaa hid"), -25);
        assert_eq!(get_class_and_id_attr_weight("aaassaa hid aaaaa"), -25);
        assert_eq!(get_class_and_id_attr_weight("hid dfsdss"), -25);

        assert_eq!(get_class_and_id_attr_weight("hId"), -25);
        assert_eq!(get_class_and_id_attr_weight("aaassaa Hid"), -25);
        assert_eq!(get_class_and_id_attr_weight("aaassaa hiD aaaaa"), -25);
        assert_eq!(get_class_and_id_attr_weight("HiD dfsdss"), -25);
    }

    #[test]
    fn test_positive_classes_weight() {
        let positives =
            "article|body|content|entry|hentry|h-entry|main|page|pagination|post|text|blog|story"
                .split('|')
                .collect::<Vec<_>>();
        for n in positives {
            let attr_value = format!("some random value {}", n);
            assert_eq!(get_class_and_id_attr_weight(attr_value.as_str()), 25);
            let attr_value = attr_value.to_uppercase();
            assert_eq!(get_class_and_id_attr_weight(attr_value.as_str()), 25);
        }
    }

    #[test]
    fn test_replace_relative_urls_with_rel_img_src_with_base() {
        const TEST_INPUT: &str = r###"<!doctype html><html><head>
        <title>Example Domain</title>
        </head>
        <body>
        <div>
        <img src="images/img.png" />
        </div>
        </body>
        </html>"###;

        let doc = parse_html(TEST_INPUT);
        replace_relative_urls_with_absolute(&doc, "http://www.example.com/world/", "..");
        let e = doc.select_first("img").unwrap();
        let node = e.as_node();
        assert_eq!(node.element_name().unwrap(), "img");
        assert_eq!(
            node.attr_value("src").unwrap(),
            "http://www.example.com/images/img.png"
        );
        assert_eq!(doc.select("img").unwrap().count(), 1);
    }

    #[test]
    fn test_replace_relative_urls_with_rel_img_src_without_base() {
        const TEST_INPUT: &str = r###"<!doctype html><html><head>
        <title>Example Domain</title>
        </head>
        <body>
        <div>
        <img src="images/img.png" />
        </div>
        </body>
        </html>"###;

        let doc = parse_html(TEST_INPUT);
        replace_relative_urls_with_absolute(&doc, "http://www.example.com/world/", "");
        let e = doc.select_first("img").unwrap();
        let node = e.as_node();
        assert_eq!(node.element_name().unwrap(), "img");
        assert_eq!(
            node.attr_value("src").unwrap(),
            "http://www.example.com/world/images/img.png"
        );
        assert_eq!(doc.select("img").unwrap().count(), 1);
    }

    #[test]
    fn test_replace_relative_urls_with_abs_img_src() {
        const TEST_INPUT: &str = r###"<!doctype html><html><head>
        <title>Example Domain</title>
        </head>
        <body>
        <div>
        <img src="https://google.com/images/img.png" />
        </div>
        </body>
        </html>"###;

        let doc = parse_html(TEST_INPUT);
        replace_relative_urls_with_absolute(&doc, "http://www.example.com/world/", "");
        let e = doc.select_first("img").unwrap();
        let node = e.as_node();
        assert_eq!(node.element_name().unwrap(), "img");
        assert_eq!(
            node.attr_value("src").unwrap(),
            "https://google.com/images/img.png"
        );
        assert_eq!(doc.select("img").unwrap().count(), 1);
    }

    #[test]
    fn test_replace_relative_urls_with_hash_link_to_self() {
        const TEST_INPUT: &str = r###"<!doctype html><html><head>
        <title>Example Domain</title>
        </head>
        <body>
        <div>
        <a href="#self_id" id="self_id">Self ID</a>
        </div>
        </body>
        </html>"###;

        let doc = parse_html(TEST_INPUT);
        assert_eq!(
            doc.select_first("#self_id")
                .unwrap()
                .as_node()
                .element_name()
                .unwrap(),
            "a"
        );
        replace_relative_urls_with_absolute(&doc, "http://www.example.com", "");
        assert_eq!(
            doc.select_first("#self_id")
                .unwrap()
                .as_node()
                .element_name()
                .unwrap(),
            "span"
        );
        assert_eq!(doc.select("a").unwrap().count(), 0);
    }

    #[test]
    fn test_replace_relative_urls_with_hash_link_to_js() {
        const TEST_INPUT: &str = r###"<!doctype html><html><head>
        <title>Example Domain</title>
        </head>
        <body>
        <div>
        <a href="javascript:" id="js_link">JS Link</a>
        </div>
        </body>
        </html>"###;

        let doc = parse_html(TEST_INPUT);
        assert_eq!(
            doc.select_first("#js_link")
                .unwrap()
                .as_node()
                .element_name()
                .unwrap(),
            "a"
        );
        replace_relative_urls_with_absolute(&doc, "http://www.example.com", "");
        assert_eq!(
            doc.select_first("#js_link")
                .unwrap()
                .as_node()
                .element_name()
                .unwrap(),
            "span"
        );
        assert_eq!(doc.select("a").unwrap().count(), 0);
    }

    #[test]
    fn test_replace_relative_urls_with_hash_link_to_other() {
        const TEST_INPUT: &str = r###"<!doctype html><html><head>
        <title>Example Domain</title>
        </head>
        <body>
        <div>
        <a href="#sib_id" id="other_id">Self ID</a>
        </div>
        </body>
        </html>"###;

        let doc = parse_html(TEST_INPUT);
        assert_eq!(
            doc.select_first("#other_id")
                .unwrap()
                .as_node()
                .element_name()
                .unwrap(),
            "a"
        );
        replace_relative_urls_with_absolute(&doc, "http://www.example.com", "");
        let e = doc.select_first("#other_id").unwrap();
        let node = e.as_node();
        assert_eq!(node.element_name().unwrap(), "a");
        assert_eq!(node.attr_value("href").unwrap(), "#sib_id");
        assert_eq!(doc.select("a").unwrap().count(), 1);
    }

    #[test]
    fn test_replace_relative_urls_with_rel_link_without_base_and_with_hash() {
        const TEST_INPUT: &str = r###"<!doctype html><html><head>
        <title>Example Domain</title>
        </head>
        <body>
        <div>
        <a href="hello_world#hash" id="hello">Self ID</a>
        </div>
        </body>
        </html>"###;

        let doc = parse_html(TEST_INPUT);
        replace_relative_urls_with_absolute(&doc, "http://www.example.com/world/", "");
        let e = doc.select_first("#hello").unwrap();
        let node = e.as_node();
        assert_eq!(node.element_name().unwrap(), "a");
        assert_eq!(
            node.attr_value("href").unwrap(),
            "http://www.example.com/world/hello_world#hash"
        );
        assert_eq!(doc.select("a").unwrap().count(), 1);
    }

    #[test]
    fn test_replace_relative_urls_with_rel_link_with_base_and_hash() {
        const TEST_INPUT: &str = r###"<!doctype html><html><head>
        <title>Example Domain</title>
        </head>
        <body>
        <div>
        <a href="hello_world#hash" id="hello">Self ID</a>
        </div>
        </body>
        </html>"###;

        let doc = parse_html(TEST_INPUT);
        replace_relative_urls_with_absolute(&doc, "http://www.example.com/world/", "../");
        let e = doc.select_first("#hello").unwrap();
        let node = e.as_node();
        assert_eq!(node.element_name().unwrap(), "a");
        assert_eq!(
            node.attr_value("href").unwrap(),
            "http://www.example.com/hello_world#hash"
        );
        assert_eq!(doc.select("a").unwrap().count(), 1);
    }

    #[test]
    fn test_replace_relative_urls_with_rel_link_with_base() {
        const TEST_INPUT: &str = r###"<!doctype html><html><head>
        <title>Example Domain</title>
        </head>
        <body>
        <div>
        <a href="hello_world" id="hello">Self ID</a>
        </div>
        </body>
        </html>"###;

        let doc = parse_html(TEST_INPUT);
        replace_relative_urls_with_absolute(&doc, "http://www.example.com/world/", "../");
        let e = doc.select_first("#hello").unwrap();
        let node = e.as_node();
        assert_eq!(node.element_name().unwrap(), "a");
        assert_eq!(
            node.attr_value("href").unwrap(),
            "http://www.example.com/hello_world"
        );
        assert_eq!(doc.select("a").unwrap().count(), 1);
    }

    #[test]
    fn test_replace_relative_urls_with_rel_link_without_base() {
        const TEST_INPUT: &str = r###"<!doctype html><html><head>
        <title>Example Domain</title>
        </head>
        <body>
        <div>
        <a href="hello_world" id="hello">Self ID</a>
        </div>
        </body>
        </html>"###;

        let doc = parse_html(TEST_INPUT);
        replace_relative_urls_with_absolute(&doc, "http://www.example.com/world/", "");
        let e = doc.select_first("#hello").unwrap();
        let node = e.as_node();
        assert_eq!(node.element_name().unwrap(), "a");
        assert_eq!(
            node.attr_value("href").unwrap(),
            "http://www.example.com/world/hello_world"
        );
        assert_eq!(doc.select("a").unwrap().count(), 1);
    }

    #[test]
    fn test_word_count() {
        assert_eq!(word_count("Hello World      Another word"), 4);
        assert_eq!(word_count("Hello ."), 2);
    }

    #[test]
    fn test_apply_with_valid_selector() {
        const TEST_INPUT: &str = r###"<!doctype html><html><head>
<title>Example Domain</title>
</head>
<body>
<div id="that_node" background="black" border="1px">
<table height="100" width="100" style="width:100%">
<tr><th>Firstname</th>
<p align="center" style="border:1px solid;">Text Here</p>
<p>Another P</p>
</tr><tr><td>Jill</td></tr>
</table>
</div>
</body>
</html>"###;
        use std::sync::atomic::{AtomicUsize, Ordering};

        let doc = parse_html(TEST_INPUT);
        let counter: AtomicUsize = AtomicUsize::new(0);
        apply(&doc, &["p", "tr"], |_, s| {
            assert!(s == "p" || s == "tr");
            counter.fetch_add(1, Ordering::Relaxed);
        });
        assert_eq!(4, counter.load(Ordering::Relaxed));
    }

    #[test]
    fn test_apply_with_invalid_selector() {
        const TEST_INPUT: &str = r###"<!doctype html><html><head>
<title>Example Domain</title>
</head>
<body>
<div id="that_node" background="black" border="1px">
<table height="100" width="100" style="width:100%">
<tr><th>Firstname</th>
<p align="center" style="border:1px solid;">Text Here</p>
<p>Another P</p>
</tr><tr><td>Jill</td></tr>
</table>
</div>
</body>
</html>"###;
        use std::sync::atomic::{AtomicUsize, Ordering};

        let doc = parse_html(TEST_INPUT);
        let counter: AtomicUsize = AtomicUsize::new(0);
        apply(&doc, &["p", "-123"], |_, s| {
            assert!(s == "p");
            counter.fetch_add(1, Ordering::Relaxed);
        });
        assert_eq!(2, counter.load(Ordering::Relaxed));
    }

    #[test]
    fn test_resolve_url_with_normal_base_and_relative() {
        let result = to_absolute_uri("index.html", "http://example.com", "");
        assert_eq!(result, "http://example.com/index.html");
    }

    #[test]
    fn test_resolve_url_with_normal_base_as_file_url_and_relative() {
        let result = to_absolute_uri("foo/bar/index.html", "http://fakehost/test/page.html", "");
        assert_eq!(result, "http://fakehost/test/foo/bar/index.html");
    }

    #[test]
    fn test_resolve_url_with_base_trailing_slash_and_normal_relative() {
        let result = to_absolute_uri("index.html", "http://example.com/", "");
        assert_eq!(result, "http://example.com/index.html");
    }

    #[test]
    fn test_resolve_url_with_normal_base_and_relative_starting_with_slash() {
        let result = to_absolute_uri("/index.html", "http://example.com", "");
        assert_eq!(result, "http://example.com/index.html");
    }

    #[test]
    fn test_resolve_url_with_base_trailing_slash_and_relative_starting_with_slash() {
        let result = to_absolute_uri("/index.html", "http://example.com/", "");
        assert_eq!(result, "http://example.com/index.html");
    }

    #[test]
    fn test_resolve_url_with_full_url() {
        let result = to_absolute_uri("http://example.com/index.html", "http://example.com/", "");
        assert_eq!(result, "http://example.com/index.html");
    }

    #[test]
    fn test_rename_tag_with_selector_with_by_id_selector() {
        const TEST_INPUT: &str = r###"<!doctype html><html><head>
<title>Example Domain</title>
</head>
<body>
<div>foo <p id="rename_it"><br>bar<br> <br><br>abc</p></div>
</body>
</html>"###;
        let doc = parse_html(TEST_INPUT);
        assert_eq!(count_elements(&doc, "br"), 4);
        assert_eq!(count_elements(&doc, "p"), 1);
        assert_eq!(count_elements(&doc, "div"), 1);
        let body = doc.select("body").unwrap().next().unwrap();
        let n = body.as_node();
        rename_tags_with_selector(n, "#rename_it", "div");
        assert_eq!(count_elements(&doc, "br"), 4);
        assert_eq!(count_elements(&doc, "p"), 0);
        assert_eq!(count_elements(&doc, "div"), 2);
    }

    #[test]
    fn test_rename_tag_with_selector_with_by_class_selector() {
        const TEST_INPUT: &str = r###"<!doctype html><html><head>
<title>Example Domain</title>
</head>
<body>
<div>foo <p class="rename_it"><br>bar<br> <br><br>abc</p>
<p class="rename_it">
nothing special in here
</p>
</div>
</body>
</html>"###;
        let doc = parse_html(TEST_INPUT);
        assert_eq!(count_elements(&doc, "br"), 4);
        assert_eq!(count_elements(&doc, "p"), 2);
        assert_eq!(count_elements(&doc, "div"), 1);
        let body = doc.select("body").unwrap().next().unwrap();
        let n = body.as_node();
        rename_tags_with_selector(n, ".rename_it", "div");
        assert_eq!(count_elements(&doc, "br"), 4);
        assert_eq!(count_elements(&doc, "p"), 0);
        assert_eq!(count_elements(&doc, "div"), 3);
    }

    #[test]
    fn test_link_to_itself_with_postivie_absolute_url() {
        let n = new_html_element("a");
        n.as_element()
            .unwrap()
            .attributes
            .borrow_mut()
            .insert("id", String::from("content"));
        let href = "http://www.something.com/#content";
        let doc_uri = "http://www.something.com/";
        assert!(link_to_itself(&n, href, doc_uri));
    }

    fn link_to_itself(node: &NodeRef, href: &str, doc_uri: &str) -> bool {
        if !href.starts_with('#') && !href.starts_with(doc_uri) {
            return false;
        }
        if let Some(id) = node.attr_value("id") {
            if href.is_empty()
                || id == href[1..]
                || (href.starts_with(doc_uri)
            // account for the hash
            && href.len() > doc_uri.len() + 1
            && id == href[doc_uri.len() + 1..])
            {
                return true;
            }
        }

        false
    }
}
