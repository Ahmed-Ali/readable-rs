use crate::parser::{NodeExt, NodeRef};
use crate::utils::{get_next_node, text_similarity};
use regex::Regex;
use std::collections::HashMap;
use std::sync::LazyLock;

pub static BY_LINE_REGEX: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r"(?i)byline|author|dateline|writtenby|p-author").unwrap());

fn is_possibly_valid_by_line(node: &NodeRef) -> bool {
    let txt = node.text_contents().trim().to_string();
    !txt.is_empty() && txt.chars().count() < 100
}

pub fn get_article_by_line(node: &NodeRef, matching_str: &str) -> Option<String> {
    let rel = node.attr_value("rel");
    let itemprop = node.attr_value("itemprop");
    let rel_is_auth = rel.unwrap_or_default() == "author";
    let itemprop_is_auth = itemprop.unwrap_or_default().contains("author");
    let regex_match = BY_LINE_REGEX.is_match(matching_str);
    let is_valid_by_line = is_possibly_valid_by_line(node);
    if (rel_is_auth || itemprop_is_auth || regex_match) && is_valid_by_line {
        let end_marker = get_next_node(node, true);
        let mut next = get_next_node(node, false);
        let mut itemprop_name_node: Option<NodeRef> = None;
        while next.is_some() && next != end_marker {
            let n = next.clone().unwrap();
            if let Some(itemprop) = n.attr_value("itemprop") {
                if itemprop.contains("name") {
                    itemprop_name_node = Some(n);
                    break;
                }
            }
            next = get_next_node(&n, false);
        }
        let byline_node = itemprop_name_node.unwrap_or_else(|| node.clone());
        return Some(byline_node.text_contents().trim().to_string());
    }

    None
}

pub fn header_duplicates_title(node: &NodeRef, article_title: &str) -> bool {
    if node.element_name() != Some("h1") && node.element_name() != Some("h2") {
        return false;
    }
    let heading = node.text_contents().trim().to_string();
    if heading.is_empty() || article_title.trim().is_empty() {
        return false;
    }
    text_similarity(article_title, heading.as_str()) > 0.75
}

fn parse_style_attrs(style: &str) -> HashMap<&str, &str> {
    let mut attrs = HashMap::new();
    style.split(';').for_each(|a| {
        let attr_split = a.split(':').collect::<Vec<_>>();
        if attr_split.len() == 2 {
            attrs.insert(attr_split[0].trim(), attr_split[1].trim());
        }
    });
    attrs
}

/// Returns true if the passed node is probably a hidden node
/// If any of the following conditions are met, the node is considered
/// to be probably hidden:
/// - If node has a "hidden" attribute
/// - If the node hsa "aria-hidden" attribute and its value is true
/// - If the node has a style attribute and its value has "display: none"
pub fn is_probably_hidden(n: &NodeRef) -> bool {
    if n.attr_value("hidden").is_some() {
        return true;
    }

    if let Some(aria_hidden) = n.attr_value("aria-hidden") {
        if aria_hidden.trim() == "true" {
            if let Some(class_name) = n.attr_value("class") {
                if class_name.contains("fallback-image") {
                    return false;
                }
            }
            return true;
        }
    }

    if let Some(style) = n.attr_value("style") {
        let style_attrs = parse_style_attrs(style.as_str());

        if let Some(display) = style_attrs.get("display") {
            if display == &"none" {
                return true;
            }
        }
        if let Some(visibility) = style_attrs.get("visibility") {
            if visibility == &"hidden" {
                return true;
            }
        }
    }

    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse_html;

    #[test]
    fn test_is_probably_hidden() {
        const TEST_INPUT: &str = r###"<!doctype html><html><head>
<title>Example Domain</title>
</head>
<body>
<div>
<p hidden id="hidden_p">This is meant to be hidden</p>
<p aria-hidden="true" id="hidden_p2">This is meant to be hidden</p>
<p style="display:none" id="hidden_p3">This is meant to be hidden</p>
<p style="display:block" aria-hidden="false" id="non_hidden">This is meant to be hidden</p>
</div>
</body>
</html>"###;

        let doc = parse_html(TEST_INPUT);
        let hidden_p = doc.select_first("#hidden_p").unwrap();
        let hidden_p2 = doc.select_first("#hidden_p2").unwrap();
        let hidden_p3 = doc.select_first("#hidden_p3").unwrap();
        let none_hidden = doc.select_first("#non_hidden").unwrap();
        assert_eq!(is_probably_hidden(hidden_p.as_node()), true);
        assert_eq!(is_probably_hidden(hidden_p2.as_node()), true);
        assert_eq!(is_probably_hidden(hidden_p3.as_node()), true);
        assert_eq!(is_probably_hidden(none_hidden.as_node()), false);
    }

    #[test]
    fn test_is_probably_hidden_with_display_none() {
        const TEST_INPUT: &str = r###"<!doctype html><html><head>
<title>Example Domain</title>

</head>
<body>
<div id="hide" style="display:none">Foo bar</div>
</body>
</html>"###;
        let document = parse_html(TEST_INPUT);
        let n = document.select_first("#hide").unwrap().as_node().clone();
        assert_eq!(is_probably_hidden(&n), true);
    }

    #[test]
    fn test_is_probably_hidden_with_hidden_attribute() {
        const TEST_INPUT: &str = r###"<!doctype html><html><head>
<title>Example Domain</title>
</head>
<body>
<div id="hide" hidden>Foo bar</div>
</body>
</html>"###;
        let document = parse_html(TEST_INPUT);
        let n = document.select_first("#hide").unwrap().as_node().clone();
        assert_eq!(is_probably_hidden(&n), true);
    }

    #[test]
    fn test_is_probably_hidden_with_aria_hidden_is_true() {
        const TEST_INPUT: &str = r###"<!doctype html><html><head>
<title>Example Domain</title>
</head>
<body>
<div id="hide" aria-hidden=true>Foo bar</div>
</body>
</html>"###;
        let document = parse_html(TEST_INPUT);
        let n = document.select_first("#hide").unwrap().as_node().clone();
        assert_eq!(is_probably_hidden(&n), true);
    }

    #[test]
    fn test_is_probably_visible_with_aria_hidden_is_false() {
        const TEST_INPUT: &str = r###"<!doctype html><html><head>
<title>Example Domain</title>
</head>
<body>
<div id="hide" aria-hidden=false>Foo bar</div>
</body>
</html>"###;
        let document = parse_html(TEST_INPUT);
        let n = document.select_first("#hide").unwrap().as_node().clone();
        assert_eq!(is_probably_hidden(&n), false);
    }
    #[test]
    fn test_is_probably_hidden_with_hide_attrs() {
        const TEST_INPUT: &str = r###"<!doctype html><html><head>
<title>Example Domain</title>
</head>
<body>
<div id="visible_div">Foo bar</div>
</body>
</html>"###;
        let document = parse_html(TEST_INPUT);
        let n = document
            .select_first("#visible_div")
            .unwrap()
            .as_node()
            .clone();
        assert_eq!(is_probably_hidden(&n), false);
    }
}
