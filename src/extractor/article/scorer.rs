use crate::*;

use crate::logging::logger::*;
use crate::logging::logging_defs::*;
use crate::node_ext::{NodeScoreStore, NodeScoreExt};
use crate::parser::{NodeExt, NodeRef, new_html_element};
use std::cmp::Ordering;
use utils::*;

/// The output of the scoring phase.
pub struct ScoringResult {
    /// The DOM node that scored highest as an article-content container.
    pub top_candidate: NodeRef,
    /// `true` when no real candidate was found and a synthetic `<div>` was
    /// created to hold the body's children as a last resort.
    pub had_to_create_top_candidate_node: bool,
}

/// Sets the initial node readability score based on its tag
/// name and class weight.
fn initialize_node_readability_score(
    node: &NodeRef,
    store: &mut NodeScoreStore,
    weight_classes: bool,
    logger: &PerfLogger,
) {
    start_span!(logger, INITIALIZE_READABILITY_SCORE);
    node.set_readability_score(store, Some(0.0));
    if let Some(name) = node.element_name() {
        match name {
            "div" => node.offset_readability_score(store, 5.0),
            "pre" | "td" | "blockquote" => node.offset_readability_score(store, 3.0),
            "address" | "ol" | "ul" | "li" | "dl" | "dd" | "dt" | "form" => {
                node.offset_readability_score(store, -3.0)
            }
            "h1" | "h2" | "h3" | "h4" | "h5" | "h6" | "th" => {
                node.offset_readability_score(store, -5.0)
            }
            _ => (),
        }
    }

    if weight_classes {
        node.offset_readability_score(store, get_class_and_id_weight(node) as f64);
    }
    end_span!(logger, INITIALIZE_READABILITY_SCORE);
}

/// Score a list of candidate elements and select the best content container.
///
/// Each element in `elements_to_score` contributes points to its ancestors
/// based on comma count, text length, and (optionally) class/id weight.
/// After all candidates are scored, the top-N are re-scored with a link-
/// density penalty, and the highest scorer (or its ancestor, if walking up
/// the tree reveals a better container) is returned as the top candidate.
pub fn score_elements(
    elements_to_score: &[NodeRef],
    store: &mut NodeScoreStore,
    weight_classes: bool,
    max_number_of_top_candidates: usize,
    body_content_node: &NodeRef,
    logger: &PerfLogger,
) -> ScoringResult {
    // Loop through all paragraphs, and assign a score to them based on how content-y they look.
    // Then add their score to their parent node.
    // A score is determined by things like number of commas,
    // class names, etc. Maybe eventually link density.
    let mut candidates: Vec<NodeRef> = vec![];
    start_span!(logger, CONTENT_SCORING);
    add_point_to_span_str!(logger, CONTENT_SCORING, "find_all_candidates_begin");
    elements_to_score.iter().for_each(|n| {
        if n.parent().is_none() || n.element_name().is_none() {
            return;
        }
        // If this paragraph is less than 25 characters, don't even count it.
        let inner_text = get_normalized_text_content(n, logger);
        let text_len = inner_text.chars().count();
        if text_len < 25 {
            return;
        }

        // Exclude nodes with no ancestor.
        let ancestors = get_node_ancestors(n, 5);
        if ancestors.is_empty() {
            return;
        }

        // Start with one point for the paragraph itself as a base.
        let mut content_score = 1;

        // Add points for any commas within this paragraph (including non-ASCII commas).
        let comma_count = COMMA_REGEX.split(inner_text.as_str()).count();
        content_score += comma_count;

        // For every 100 characters in this paragraph, add another point.
        // Up to 3 points.
        let text_len_by_100 = (inner_text.chars().count() as f64 / 100.0).floor() as usize;
        content_score += text_len_by_100.min(3);
        // Initialize and score ancestors.
        ancestors.iter().enumerate().for_each(|(level, ancstr)| {
            if ancstr.element_name().is_none()
                || ancstr.parent().is_none()
                || ancstr.parent().unwrap().element_name().is_none()
            {
                return;
            }

            if ancstr.readability_score(store).is_none() {
                initialize_node_readability_score(ancstr, store, weight_classes, logger);
                candidates.push(ancstr.clone());
            }

            // Node score divider:
            // - parent:             1 (no division)
            // - grandparent:        2
            // - great grandparent+: ancestor level * 3
            let divider = if level == 0 {
                1.0
            } else if level == 1 {
                2.0
            } else {
                level as f64 * 3.0
            };

            ancstr.offset_readability_score(store, content_score as f64 / divider);
        });
    });
    add_point_to_span_str!(logger, CONTENT_SCORING, "find_all_candidates_end");
    // After we've calculated scores, loop through all of the possible
    // candidate nodes we found and find the one with the highest score.
    add_point_to_span_str!(
        logger,
        CONTENT_SCORING,
        "find_potential_top_candidates_begin"
    );
    let top_candidates = get_top_candidates(&candidates, store, max_number_of_top_candidates, logger);
    add_point_to_span_str!(logger, CONTENT_SCORING, "find_potential_top_candidates_end");

    add_point_to_span_str!(logger, CONTENT_SCORING, "find_top_candidate_begin");
    let mut had_to_create_top_candidate_node = false;
    let mut parent_of_top_candidate: Option<NodeRef>;
    let mut tc = new_html_element("div");

    // If we still have no top candidate, just use the body as a last resort.
    // We also have to copy the body node so it is something we can modify.
    let tc_opt = top_candidates.get(0);
    if tc_opt.is_none() || tc_opt.unwrap().element_name() == Some("body") {
        // Move all of the page's children into top_candidate
        had_to_create_top_candidate_node = true;
        // Move everything (not just elements, also text nodes etc.) into the container
        // so we even include text directly in the body:
        move_children(body_content_node, &tc);
        body_content_node.append(tc.clone());
        initialize_node_readability_score(&tc, store, weight_classes, logger);
    } else if let Some(t) = tc_opt {
        // Find a better top candidate node if it contains (at least three) nodes which belong to `top_candidates` vec
        // and whose scores are quite closed with current `tc` node.
        tc = t.clone();
        let mut alternative_candidate_ancestors: Vec<Vec<NodeRef>> = vec![];
        top_candidates.iter().for_each(|c| {
            if c == &tc {
                return;
            }
            if c.readability_score(store).unwrap() / tc.readability_score(store).unwrap() >= 0.75 {
                alternative_candidate_ancestors
                    .push(get_node_ancestors(c, DEFAULT_MAX_ANCESTORS_DEPTH))
            }
        });

        let minimum_top_candidates = 3;
        if alternative_candidate_ancestors.len() >= minimum_top_candidates {
            parent_of_top_candidate = tc.parent();
            while parent_of_top_candidate.is_some()
                && parent_of_top_candidate.clone().unwrap().element_name() != Some("body")
            {
                let mut lists_containing_this_ancestor = 0;
                for list in &alternative_candidate_ancestors {
                    if list.contains(&parent_of_top_candidate.clone().unwrap()) {
                        lists_containing_this_ancestor += 1;
                    }
                }

                if lists_containing_this_ancestor >= minimum_top_candidates {
                    tc = parent_of_top_candidate.unwrap();
                    break;
                }

                parent_of_top_candidate = parent_of_top_candidate.unwrap().parent();
            }
        }

        if tc.readability_score(store).is_none() {
            initialize_node_readability_score(&tc, store, weight_classes, logger);
        }

        // Because of our bonus system, parents of candidates might have scores
        // themselves. They get half of the node. There won't be nodes with higher
        // scores than our topCandidate, but if we see the score going *up* in the first
        // few steps up the tree, that's a decent sign that there might be more content
        // lurking in other places that we want to unify in. The sibling stuff
        // below does some of that - but only if we've looked high enough up the DOM
        // tree.

        parent_of_top_candidate = tc.parent();
        let mut last_score = tc.readability_score(store).unwrap();
        // The scores shouldn't get too low.
        let score_threshold = last_score / 3.0;

        while parent_of_top_candidate.is_some()
            && parent_of_top_candidate.clone().unwrap().element_name() != Some("body")
        {
            let potc = parent_of_top_candidate.clone().unwrap();
            if potc.readability_score(store).is_none() {
                parent_of_top_candidate = potc.parent();
                continue;
            }
            let parent_score = potc.readability_score(store).unwrap();
            if parent_score < score_threshold {
                break;
            }
            if parent_score > last_score {
                // Alright! We found a better parent to use.
                tc = potc;
                break;
            }
            last_score = potc.readability_score(store).unwrap_or(0.0);
            parent_of_top_candidate = potc.parent();
        }

        // If the top candidate is the only child, use parent instead. This will help sibling
        // joining logic when adjacent content is actually located in parent's sibling node.
        parent_of_top_candidate = tc.parent();

        while parent_of_top_candidate.is_some()
            && parent_of_top_candidate.clone().unwrap().element_name() != Some("body")
            && parent_of_top_candidate
                .clone()
                .unwrap()
                .element_children()
                .len()
                == 1
        {
            tc = parent_of_top_candidate.clone().unwrap();
            parent_of_top_candidate = tc.parent();
        }

        if tc.readability_score(store).is_none() {
            initialize_node_readability_score(&tc, store, weight_classes, logger);
        }
    }
    add_point_to_span_str!(logger, CONTENT_SCORING, "find_top_candidate_end");
    end_span!(logger, CONTENT_SCORING);
    ScoringResult {
        top_candidate: tc,
        had_to_create_top_candidate_node,
    }
}

fn get_top_candidates(
    all_candidates: &[NodeRef],
    store: &mut NodeScoreStore,
    max_number_of_top_candidates: usize,
    logger: &PerfLogger,
) -> Vec<NodeRef> {
    start_span!(logger, CS_GET_TOP_CANDIDATES);
    let mut top_candidates: Vec<NodeRef> = vec![];
    annotate_span!(
        logger,
        CS_GET_TOP_CANDIDATES,
        format!(
            "Will calculate readability for: {} candidate",
            all_candidates.len()
        )
    );
    add_point_to_span_str!(
        logger,
        CS_GET_TOP_CANDIDATES,
        "calculate_candidates_score_begin"
    );
    for c in all_candidates {
        recalculate_candidate_readability_score_using_link_density(&c, store, logger);
        top_candidates.push(c.clone());
    }
    add_point_to_span_str!(
        logger,
        CS_GET_TOP_CANDIDATES,
        "calculate_candidates_score_end"
    );
    add_point_to_span_str!(logger, CS_GET_TOP_CANDIDATES, "sort_candidates_begin");
    top_candidates.sort_by(|lhs, rhs| {
        let rrs = rhs.readability_score(store).unwrap();
        let lrs = lhs.readability_score(store).unwrap();
        if let Some(ord) = rrs.partial_cmp(&lrs) {
            return ord;
        }
        Ordering::Equal
    });
    add_point_to_span_str!(logger, CS_GET_TOP_CANDIDATES, "sort_candidates_end");
    add_point_to_span_str!(logger, CS_GET_TOP_CANDIDATES, "pick_top_n_candidates_begin");
    top_candidates.truncate(max_number_of_top_candidates);
    add_point_to_span_str!(logger, CS_GET_TOP_CANDIDATES, "pick_top_n_candidates_end");
    top_candidates
}

fn recalculate_candidate_readability_score_using_link_density(
    candidate: &NodeRef,
    store: &mut NodeScoreStore,
    logger: &PerfLogger,
) {
    // Scale the final candidates score based on link density. Good content
    // should have a relatively small link density (5% or less) and be mostly
    // unaffected by this operation.
    start_span!(logger, CS_CALC_READABILITY_SCORE_WITH_LINK_DENSITY);
    let link_density = get_link_density(candidate, logger);
    end_span!(logger, CS_CALC_READABILITY_SCORE_WITH_LINK_DENSITY);
    let candidate_score = candidate.readability_score(store).unwrap() * (1.0 - link_density);
    candidate.set_readability_score(store, Some(candidate_score));
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::logging::logger::PerfLogger;
    use crate::parser::parse_html;

    fn new_logger() -> PerfLogger {
        PerfLogger::new(vec![])
    }

    #[test]
    fn score_elements_sets_scores_for_candidates() {
        let doc = parse_html(
            "<body><div><p>This is a long enough paragraph to be scored properly.</p></div></body>",
        );
        let p = doc.select_first("p").unwrap().as_node().clone();
        let body = doc.select_first("body").unwrap().as_node().clone();
        let logger = new_logger();
        let mut store = NodeScoreStore::default();
        let res = score_elements(&[p], &mut store, true, 5, &body, &logger);
        assert!(res.top_candidate.readability_score(&store).is_some());
    }

    #[test]
    fn score_elements_ignores_orphan_nodes_without_panic() {
        let doc = parse_html("<p>This is a long enough paragraph to be scored properly.</p>");
        let orphan = doc.select_first("p").unwrap().as_node().clone();
        orphan.detach();
        let body = new_html_element("body");
        let logger = new_logger();
        let mut store = NodeScoreStore::default();
        let _ = score_elements(&[orphan], &mut store, true, 5, &body, &logger);
    }
}
