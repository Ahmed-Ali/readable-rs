mod utils;

use crate::logging::logger::*;
use crate::logging::logging_defs::*;
use crate::parser::{new_html_element, NodeExt, NodeRef};
use crate::utils::*;
use std::collections::HashSet;
use std::sync::LazyLock;
use utils::*;

pub static MUST_BE_NOT_EMPTY_TAGS: LazyLock<HashSet<&'static str>> =
    LazyLock::new(|| HashSet::from(["div", "section", "h1", "h2", "h3", "h4", "h5", "h6"]));

// Element tags to score by default.
pub static DEFAULT_ELEMENTS_TO_SCORE: LazyLock<HashSet<&'static str>> =
    LazyLock::new(|| HashSet::from(["section", "h2", "h3", "h4", "h5", "h6", "p", "td", "pre"]));

// In the original algorithm, this is called: DIV_TO_P_ELEMS
pub static CONTENT_BLOCK_ELEMENTS: LazyLock<Vec<&'static str>> = LazyLock::new(|| {
    vec![
        "blockquote", "dl", "div", "img", "ol", "p", "pre", "table", "ul",
    ]
});

/// First, node prepping. Trash nodes that look cruddy (like ones with the
/// class name "comment", etc), and turn divs into P tags where they have been
/// used inappropriately (as in, where they contain no other block level elements.)
/// Then returns approperiate elements to be used for scoring
#[allow(clippy::cognitive_complexity)]
pub fn clean(
    doc_node: &NodeRef,
    by_line: &mut Option<String>,
    strip_unlikely_candidates: bool,
    article_title: &str,
    logger: &PerfLogger,
) -> Vec<NodeRef> {
    let mut elements_to_score: Vec<NodeRef> = vec![];
    let mut node = Some(doc_node.clone());
    let mut should_remove_title_header = !article_title.trim().is_empty();
    start_span!(logger, CONTENT_CLEANUP);
    while node.is_some() {
        add_point_to_span_str!(
            logger,
            CONTENT_CLEANUP,
            "marking_start_of_content_cleanup_loop"
        );
        let unwrapped_node = node.clone().unwrap();

        // in some instances, some "a" elements used to be self referencing
        // while they match the "is_probably_hidden" criteria
        // removing them, will cause dangling reference in other places
        if is_probably_hidden(&unwrapped_node)
            && !(unwrapped_node.element_name() == Some("a")
                && unwrapped_node.attr_value("id").is_some())
        {
            node = remove_and_get_next(&unwrapped_node);
            add_point_to_span_str!(logger, CONTENT_CLEANUP, "early_pass_in_is_probably_hidden");
            continue;
        }
        add_point_to_span_str!(logger, CONTENT_CLEANUP, "past_is_probably_hidden");

        let match_string = match_string_for_node(&unwrapped_node);
        add_point_to_span_str!(logger, CONTENT_CLEANUP, "fetched_matching_str_for_the_node");

        if unwrapped_node.attr_value("aria-modal").as_deref() == Some("true")
            && unwrapped_node.attr_value("role").as_deref() == Some("dialog")
        {
            node = remove_and_get_next(&unwrapped_node);
            add_point_to_span_str!(logger, CONTENT_CLEANUP, "early_pass_in_modal_dialog");
            continue;
        }

        // Check to see if this node is a byline, and remove it if it is.
        if by_line.is_none() {
            *by_line = get_article_by_line(&unwrapped_node, match_string.as_str());
            if by_line.is_some() {
                node = remove_and_get_next(&unwrapped_node);
                add_point_to_span_str!(
                    logger,
                    CONTENT_CLEANUP,
                    "early_pass_in_check_and_set_article_by_line"
                );
                continue;
            }
        }

        add_point_to_span_str!(
            logger,
            CONTENT_CLEANUP,
            "past_check_and_set_article_by_line"
        );

        if should_remove_title_header && header_duplicates_title(&unwrapped_node, article_title) {
            node = remove_and_get_next(&unwrapped_node);
            should_remove_title_header = false;
            add_point_to_span_str!(
                logger,
                CONTENT_CLEANUP,
                "early_pass_in_header_duplicates_title"
            );
            continue;
        }

        // Remove unlikely candidates
        if strip_unlikely_candidates {
            if let Some(n) = strip_unlikely_and_get_next(&unwrapped_node, match_string.as_str()) {
                node = Some(n);
                add_point_to_span_str!(
                    logger,
                    CONTENT_CLEANUP,
                    "early_pass_in_strip_unlikely_candidates"
                );
                continue;
            }

            if let Some(role) = unwrapped_node.attr_value("role") {
                if UNLIKELY_ROLES.contains(role.as_str()) {
                    node = remove_and_get_next(&unwrapped_node);
                    add_point_to_span_str!(
                        logger,
                        CONTENT_CLEANUP,
                        "early_pass_in_unlikely_roles"
                    );
                    continue;
                }
            }
        }

        add_point_to_span_str!(logger, CONTENT_CLEANUP, "past_strip_unlikely_candidates");
        let node_name = unwrapped_node.element_name().unwrap_or_default();

        // Remove DIV, SECTION, and HEADER nodes without any content(e.g. text, image, video, or iframe).
        if MUST_BE_NOT_EMPTY_TAGS.contains(node_name)
            && is_element_without_content(&unwrapped_node)
        {
            node = remove_and_get_next(&unwrapped_node);
            add_point_to_span_str!(
                logger,
                CONTENT_CLEANUP,
                "early_pass_in_MUST_BE_NOT_EMPTY_TAGS"
            );
            continue;
        }
        add_point_to_span_str!(logger, CONTENT_CLEANUP, "past_MUST_BE_NOT_EMPTY_TAGS");

        if DEFAULT_ELEMENTS_TO_SCORE.contains(node_name) {
            elements_to_score.push(unwrapped_node.clone());
        }

        add_point_to_span_str!(logger, CONTENT_CLEANUP, "past_DEFAULT_ELEMENTS_TO_SCORE");

        if node_name == "div" {
            add_point_to_span_str!(logger, CONTENT_CLEANUP, "before_process_div_node");
            if let Some(n) =
                process_div_node(unwrapped_node.clone(), &mut elements_to_score, logger)
            {
                node = Some(n);
            }
            add_point_to_span_str!(logger, CONTENT_CLEANUP, "after_process_div_node");
        }

        node = get_next_node(&node.clone().unwrap(), false);
    }
    end_span!(logger, CONTENT_CLEANUP);

    elements_to_score
}

/// Turn all divs that don't have children block level elements into p's
fn process_div_node(
    node: NodeRef,
    elements_to_score: &mut Vec<NodeRef>,
    logger: &PerfLogger,
) -> Option<NodeRef> {
    let mut result_node: Option<NodeRef> = None;
    // Put phrasing content into paragraphs.
    let mut child = node.first_child();
    while let Some(current) = child {
        if is_phrasing_content(&current) {
            let p = new_html_element("p");
            let mut next = Some(current.clone());
            while let Some(n) = next.clone() {
                if !is_phrasing_content(&n) {
                    break;
                }
                let next_sibling = n.next_sibling();
                p.append(n);
                next = next_sibling;
            }

            while let Some(first) = p.first_child() {
                if is_whitespace_node(&first) {
                    first.detach();
                } else {
                    break;
                }
            }
            while let Some(last) = p.last_child() {
                if is_whitespace_node(&last) {
                    last.detach();
                } else {
                    break;
                }
            }

            if p.first_child().is_some() {
                if let Some(insertion_point) = next.clone() {
                    insertion_point.insert_before(p.clone());
                } else {
                    node.append(p.clone());
                }
            }

            child = next;
            continue;
        }
        child = current.next_sibling();
    }

    // Sites like http://mobile.slate.com encloses each paragraph with a DIV
    // element. DIVs with only a P element inside and no text content can be
    // safely converted into plain P elements to avoid confusing the scoring
    // algorithm with DIVs which are, in practice, paragraphs.
    if contains_single_tag_in_element(&node, "p") && get_link_density(&node, logger) < 0.25 {
        // This paragraph doesn't have any siblings
        // Let's put it in its parent place
        // as its parent is useless
        let single_paragraph_node = node.first_element_child().unwrap();
        node.insert_after(single_paragraph_node.clone());
        node.detach();
        elements_to_score.push(single_paragraph_node.clone());
        result_node = Some(single_paragraph_node);
    } else if !node_contains_any_tag_of(&node, &CONTENT_BLOCK_ELEMENTS) {
        let new_node = node.clone_and_rename_element("p");
        elements_to_score.push(new_node.clone());
        result_node = Some(new_node);
    }

    result_node
}
