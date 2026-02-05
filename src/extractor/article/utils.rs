use crate::logging::logger::*;
use crate::node_ext::{NodeScoreStore, NodeScoreExt};
use crate::parser::{NodeExt, NodeRef};
use crate::utils::*;

/// Cleans known presentation attributes
/// Such as align, background, etc
pub fn clean_presentation_styles(node: &NodeRef) {
    if node.as_element().is_none() || node.element_name() == Some("svg") {
        return;
    }

    let e = node.as_element().unwrap();
    let mut attrs = e.attributes.borrow_mut();
    for p_attr in PRESENTATIONAL_ATTRIBUTES.iter().cloned() {
        attrs.remove(p_attr);
    }
    if let Some(name) = node.element_name() {
        if DEPRECATED_SIZE_ATTRIBUTE_ELEMS.contains(name) {
            attrs.remove("width");
            attrs.remove("height");
        }
    }

    let mut c = node.first_element_child();
    while let Some(cu) = c {
        clean_presentation_styles(&cu);
        c = cu.next_element_sibling();
    }
}

/// Return a tuple indicating how many rows and columns this table has.
/// return.0 is rows and return.1 is cols
pub fn count_rows_and_cols_in_node(node: &NodeRef) -> (u64, u64) {
    let mut rows = 0;
    let mut cols = 0;
    if node.element_name() != Some("table") {
        // return early...
        return (rows, cols);
    }
    node.select("tr").unwrap().for_each(|tr| {
        let trn = tr.as_node();
        if let Some(rs) = trn.attr_value("rowspan") {
            if let Ok(rs) = rs.parse::<u64>() {
                rows += rs;
            } else {
                rows += 1;
            }
        } else {
            rows += 1;
        }

        // Now look for column-related info
        let mut columns_in_this_row = 0;
        trn.select("td").unwrap().for_each(|cell| {
            let celln = cell.as_node();
            if let Some(cs) = celln.attr_value("colspan") {
                if let Ok(cs) = cs.parse::<u64>() {
                    columns_in_this_row += cs;
                } else {
                    columns_in_this_row += 1;
                }
            } else {
                columns_in_this_row += 1;
            }
        });

        cols = cols.max(columns_in_this_row);
    });
    (rows, cols)
}

/// Look for 'data' (as opposed to 'layout') tables, for which we use
/// similar checks as
/// https://dxr.mozilla.org/mozilla-central/rev/71224049c0b52ab190564d3ea0eab089a159a4cf/accessible/html/HTMLTableAccessible.cpp#920
pub fn mark_data_tables_in_node(node: &NodeRef, store: &mut NodeScoreStore) {
    let debug_tables = std::env::var("READABILITY_DEBUG_TABLES").is_ok();
    node.select("table").unwrap().for_each(|table| {
        let table_node = table.as_node();
        let role = table_node
            .attr_value("role")
            .unwrap_or_default()
            .trim()
            .to_string();
        let datatable = table_node
            .attr_value("datatable")
            .unwrap_or_default()
            .trim()
            .to_string();
        if role == "presentation" {
            if debug_tables {
                eprintln!("table: role=presentation -> layout");
            }
            table_node.set_readability_data_table(store, false);
            return;
        }
        if datatable == "0" {
            if debug_tables {
                eprintln!("table: datatable=0 -> layout");
            }
            table_node.set_readability_data_table(store, false);
            return;
        }
        let summary = table_node
            .attr_value("summary")
            .unwrap_or_default()
            .trim()
            .to_string();
        if !summary.is_empty() {
            if debug_tables {
                eprintln!("table: summary -> data");
            }
            table_node.set_readability_data_table(store, true);
            return;
        }

        if let Ok(c) = table_node.select_first("caption") {
            if c.as_node().first_child().is_some() {
                if debug_tables {
                    eprintln!("table: caption -> data");
                }
                table_node.set_readability_data_table(store, true);
                return;
            }
        }

        // If the table has a descendant with any of these tags, consider a data table:
        let data_table_elements = vec!["col", "colgroup", "tfoot", "thead", "th"];
        let descendant_exists = node_contains_any_tag_of(table_node, &data_table_elements);
        if descendant_exists {
            if debug_tables {
                eprintln!("table: data tags -> data");
            }
            table_node.set_readability_data_table(store, true);
            return;
        }

        // Nested tables indicate a layout table:
        if table_node
            .select("table")
            .unwrap()
            .any(|t| t.as_node() != table_node)
        {
            if debug_tables {
                eprintln!("table: nested table -> layout");
            }
            table_node.set_readability_data_table(store, false);
            return;
        }

        let (rows, cols) = count_rows_and_cols_in_node(table_node);
        // Single column/row tables are commonly used for layout purposes.
        if rows == 1 || cols == 1 {
            if debug_tables {
                eprintln!("table: rows/cols {}x{} -> layout", rows, cols);
            }
            table_node.set_readability_data_table(store, false);
            return;
        }

        if rows >= 10 || cols > 4 {
            if debug_tables {
                eprintln!("table: rows/cols {}x{} -> data", rows, cols);
            }
            table_node.set_readability_data_table(store, true);
            return;
        }

        if debug_tables {
            eprintln!("table: rows/cols {}x{} -> {}", rows, cols, (rows * cols) > 10);
        }
        table_node.set_readability_data_table(store, (rows * cols) > 10);
    });
}

/// Returns true if the top_candidate sibling
/// have enough content score to be included in the final
/// readable content
pub fn should_append_sibling(
    sibling: &NodeRef,
    top_candidate: &NodeRef,
    logger: &PerfLogger,
    store: &NodeScoreStore,
) -> bool {
    let mut content_bonus = 0.0;
    let sibling_score_threshold =
        10.0_f64.max(top_candidate.readability_score(store).unwrap() * 0.2);
    // Give a bonus if sibling nodes and top candidates have the example same classname
    let tc_class_name = top_candidate.attr_value("class");
    if tc_class_name.is_some() && tc_class_name == sibling.attr_value("class") {
        content_bonus += top_candidate.readability_score(store).unwrap() * 0.2;
    }

    if sibling.readability_score(store).is_some()
        && ((sibling.readability_score(store).unwrap() + content_bonus) >= sibling_score_threshold)
    {
        return true;
    } else if sibling.element_name() == Some("p") {
        let link_density = get_link_density(sibling, logger);
        let node_content = get_normalized_text_content(sibling, logger);
        let node_length = node_content.chars().count();
        if node_length > 80 && link_density < 0.25 {
            return true;
        } else {
            return node_length < 80
                && node_length > 0
                && link_density == 0.0
                && crate::utils::SENTENCE_END.is_match(node_content.as_str());
        }
    }

    false
}

fn get_char_count_from_text(txt: &str, c: char) -> usize {
    let pieces: Vec<_> = txt.split(c).collect();
    pieces.len() - 1
}

fn get_text_density(node: &NodeRef, tags: &[&str], logger: &PerfLogger) -> f64 {
    let text_length = get_normalized_text_content(node, logger).len();
    if text_length == 0 {
        return 0.0;
    }
    let mut children_length = 0;
    for tag in tags {
        for child in select_descendants(node, tag) {
            children_length += get_normalized_text_content(&child, logger).len();
        }
    }
    children_length as f64 / text_length as f64
}

pub fn is_safe_to_remove_node(
    node: &NodeRef,
    tag: &str,
    is_list: bool,
    link_density_modifier: f64,
    logger: &PerfLogger,
    store: &NodeScoreStore,
) -> bool {
    // Gather counts for other typical elements embedded within.
    // Traverse backwards so we can remove nodes at the same time
    // without effecting the traversal.
    //
    // TODO: Consider taking into account original contentScore here.

    // First check if this node IS data table, in which case don't remove it.
    if tag == "table" && node.is_readability_data_table(store) {
        return false;
    }

    // Next check if we're inside a data table, in which case don't remove it as well.
    if has_ancestor_tag_with_predicate(
        node,
        "table",
        DEFAULT_MAX_ANCESTORS_DEPTH,
        |n| n.is_readability_data_table(store),
    ) {
        return false;
    }

    if has_ancestor_tag(node, "code", DEFAULT_MAX_ANCESTORS_LOOKUP_DEPTH) {
        return false;
    }

    if select_descendants(node, "table")
        .iter()
        .any(|t| t.is_readability_data_table(store))
    {
        return false;
    }

    let weight = get_class_and_id_weight(node);
    if weight < 0 {
        return true;
    }
    let node_content = get_normalized_text_content(node, logger);
    if get_char_count_from_text(node_content.as_str(), ',') < 10 {
        // If there are not very many commas, and the number of
        // non-paragraph elements is more than paragraphs or other
        // ominous signs, remove the element.
        let p = select_descendants(node, "p").len();
        let img = select_descendants(node, "img").len();
        let li_count = select_descendants(node, "li").len();
        let li = li_count as i64 - 100;
        let input = select_descendants(node, "input").len();
        let embeds = concate_nodes_with_selectors(node, vec!["object", "embed", "iframe"]);

        let mut embed_count = 0;
        for n in embeds {
            if let Some(en) = n.element_name() {
                if is_possibly_useful_video_node(&n, en) {
                    return false;
                }
            }
            embed_count += 1;
        }

        if matches_ad_or_loading(node_content.as_str()) {
            return true;
        }

        let textish_tags = vec![
            "span", "li", "td", "blockquote", "dl", "div", "img", "ol", "p", "pre", "table", "ul",
        ];
        let text_density = get_text_density(node, textish_tags.as_slice(), logger);
        let heading_density =
            get_text_density(node, &["h1", "h2", "h3", "h4", "h5", "h6"], logger);
        let link_density = get_link_density(node, logger);
        let content_len = node_content.chars().count();
        let has_figure_ancestor =
            has_ancestor_tag(node, "figure", DEFAULT_MAX_ANCESTORS_LOOKUP_DEPTH);

        let mut is_list = is_list;
        if !is_list && content_len > 0 {
            let mut list_length = 0;
            for list in concate_nodes_with_selectors(node, vec!["ul", "ol"]) {
                list_length += get_normalized_text_content(&list, logger).len();
            }
            if (list_length as f64 / content_len as f64) > 0.9 {
                is_list = true;
            }
        }

        let mut remove = false;
        if !has_figure_ancestor && img > 1 && (p as f64 / img as f64) < 0.5 {
            remove = true;
        }
        if !is_list && li > p as i64 {
            remove = true;
        }
        if input > (p as f64 / 3.0).floor() as usize {
            remove = true;
        }
        if !is_list
            && !has_figure_ancestor
            && heading_density < 0.9
            && content_len < 25
            && (img == 0 || img > 2)
            && link_density > 0.0
        {
            remove = true;
        }
        if !is_list && weight < 25 && link_density > 0.2 + link_density_modifier {
            remove = true;
        }
        if weight >= 25 && link_density > 0.5 + link_density_modifier {
            remove = true;
        }
        if (embed_count == 1 && content_len < 75) || embed_count > 1 {
            remove = true;
        }
        if img == 0 && text_density == 0.0 {
            remove = true;
        }

        if is_list && remove {
            for child in node.element_children() {
                if child.element_children().len() > 1 {
                    return remove;
                }
            }
            if img == li_count {
                return false;
            }
        }

        return remove;
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse_html;

    #[test]
    fn test_clean_presentation_styles() {
        const TEST_INPUT: &str = r###"<!doctype html><html><head>
<title>Example Domain</title>
</head>
<body>
<div id="that_node" background="black" border="1px">
<p align="center" style="border:1px solid;">Text Here</p>
<table height="100" width="100" style="width:100%"><tr><th>Firstname</th>
</tr><tr><td>Jill</td></tr></table></div>
</body>
</html>"###;

        let expected_output: &'static str = r##"<div id="that_node">
<p>Text Here</p>
<table><tbody><tr><th>Firstname</th>
</tr><tr><td>Jill</td></tr></tbody></table></div>"##;
        let doc = parse_html(TEST_INPUT);
        let e = doc.select("#that_node").unwrap().next().unwrap();
        let node = e.as_node();
        clean_presentation_styles(node);
        assert_eq!(expected_output, node.to_string());
    }
}
