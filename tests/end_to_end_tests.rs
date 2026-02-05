#[cfg(test)]
mod tests {
    use readable_rs::parser::*;

    use readable_rs::*;
    use serde::{Deserialize, Serialize};

    use std::fs::File;
    use std::io::Read;
    use std::path::Path;
    use test_generator::test_resources;

    #[derive(Serialize, Deserialize, Debug)]
    #[serde(rename_all = "camelCase")]
    struct Metadata {
        pub title: String,
        #[serde(default)]
        pub byline: Option<String>,
        #[serde(default)]
        pub dir: Option<String>,
        #[serde(default)]
        pub site_name: Option<String>,
        #[serde(default)]
        pub excerpt: Option<String>,
        #[serde(default)]
        pub published_time: Option<String>,
    }

    const TEST_TEXTURE_DIR: &str = "./";

    fn collapse_whitespace(input: &str) -> String {
        return regex::Regex::new(r"\s+")
            .unwrap()
            .replace_all(input, " ")
            .trim()
            .to_string();
    }

    fn equal_trees(expected: &NodeRef, actual: &NodeRef) -> bool {
        let mut stack: Vec<(NodeRef, NodeRef, String)> =
            vec![(expected.clone(), actual.clone(), "root".to_string())];

        let summarize = |n: &NodeRef| {
            if let Some(name) = n.element_name() {
                name.to_string()
            } else if n.as_text().is_some() {
                let t = n.text_contents();
                let t = t.trim().chars().take(30).collect::<String>();
                format!("text:{}", t)
            } else if n.as_comment().is_some() {
                "comment".to_string()
            } else {
                "node".to_string()
            }
        };

        while let Some((expected_node, actual_node, path)) = stack.pop() {
            if expected_node.element_name() != actual_node.element_name() {
                println!(
                    "Failed: element names don't match at {}: {:#?}\n\n{:#?}",
                    path,
                    expected_node.element_name(),
                    actual_node.element_name()
                );
                return false;
            }

            match (expected_node.as_element(), actual_node.as_element()) {
                (Some(l), Some(r)) => {
                    if l.attributes.borrow().map != r.attributes.borrow().map {
                        println!(
                            "Failed: attributes don't match at {}: Expected: {:#?}\n\nActual:{:#?}",
                            path,
                            l.attributes.borrow().map,
                            r.attributes.borrow().map
                        );
                        return false;
                    }
                }
                (Some(_), None) | (None, Some(_)) => {
                    println!(
                        "Failed: node types don't match at {}: {:#?}\n\nActual:{:#?}",
                        path, expected_node, actual_node
                    );
                    return false;
                }
                (None, None) => (),
            }

            match (expected_node.as_text(), actual_node.as_text()) {
                (Some(_), Some(_)) => {
                    let expected_text = collapse_whitespace(&expected_node.text_contents());
                    let actual_text = collapse_whitespace(&actual_node.text_contents());
                    if expected_text != actual_text {
                        println!(
                            "Failed: text nodes doesn't match at {}: Expected: {} \n\n Actual: {}",
                            path, expected_text, actual_text
                        );
                        return false;
                    }
                    continue;
                }
                (Some(_), None) | (None, Some(_)) => {
                    println!(
                        "Failed: node types don't match at {}: {:#?}\n\nActual:{:#?}",
                        path, expected_node, actual_node
                    );
                    return false;
                }
                (None, None) => (),
            }

            let expected_children = expected_node
                .children()
                .filter(|n| n.as_comment().is_none())
                .filter(|n| !n.text_contents().trim().is_empty())
                .collect::<Vec<_>>();
            let actual_children = actual_node
                .children()
                .filter(|n| n.as_comment().is_none())
                .filter(|n| !n.text_contents().trim().is_empty())
                .collect::<Vec<_>>();
            if expected_children.len() != actual_children.len() {
                let expected_names = expected_children
                    .iter()
                    .map(|n| summarize(n))
                    .collect::<Vec<_>>();
                let actual_names = actual_children
                    .iter()
                    .map(|n| summarize(n))
                    .collect::<Vec<_>>();
                println!(
                    "Failed: child counts don't match at {}: {:#?} vs {:#?}\nExpected: {:?}\nActual: {:?}",
                    path,
                    expected_children.len(),
                    actual_children.len(),
                    expected_names,
                    actual_names
                );
                return false;
            }

            for (i, expected_child_node) in expected_children.iter().enumerate() {
                let actual_child_node = actual_children.get(i).unwrap();
                let child_path = format!("{}/{}", path, i);
                stack.push((
                    expected_child_node.clone(),
                    actual_child_node.clone(),
                    child_path,
                ));
            }
        }

        true
    }

    fn document_root(node: &NodeRef) -> NodeRef {
        if let Ok(html) = node.select_first("html") {
            return html.as_node().clone();
        }
        if let Some(child) = node.first_child() {
            return child;
        }
        node.clone()
    }

    fn remove_comment_nodes(node: &NodeRef) {
        let descendants: Vec<_> = node.descendants().collect();
        for n in descendants {
            if n.as_comment().is_some() {
                n.detach();
            }
        }
    }

    pub fn html_contents_are_equal(expected: &str, actual: &str) -> bool {
        let e_norm = parse_html(expected);
        let a_norm = parse_html(actual);
        remove_comment_nodes(&e_norm);
        remove_comment_nodes(&a_norm);

        let e_root = document_root(&e_norm);
        let a_root = document_root(&a_norm);
        equal_trees(&e_root, &a_root)
    }

    fn test(resource: &str) {
        let source = get_source_from_dir(resource);
        let mut options = ExtractOptions::default();
        options.keep_classes = false;

        options.classes_to_preserve.insert(String::from("caption"));
        let result = extract(
            source.as_str(),
            "http://fakehost/test/page.html",
            options,
        );
        let metadata = get_expected_metadata_from_dir(resource);
        assert_eq!(result.title, metadata.title);
        assert_eq!(result.by_line, metadata.byline.unwrap_or_default());
        assert_eq!(result.dir, metadata.dir.unwrap_or_default());
        assert_eq!(result.sitename, metadata.site_name.unwrap_or_default());
        assert_eq!(result.excerpt, metadata.excerpt.unwrap_or_default());
        assert_eq!(
            result.published_time,
            metadata.published_time.unwrap_or_default()
        );
        let content = result.content.unwrap().inner_html();
        let expected = get_expected_from_dir(resource);
        let equal = html_contents_are_equal(expected.as_str(), content.as_str());

        assert!(equal);
    }

    fn get_file_content(file_path: &str) -> String {
        let path = Path::new(file_path);
        let mut content = String::new();
        let mut file = File::open(path).unwrap();
        file.read_to_string(&mut content).unwrap();
        content
    }

    fn get_source_from_dir(dir: &str) -> String {
        let fil_path = format!("{}{}/source.html", TEST_TEXTURE_DIR, dir);
        get_file_content(fil_path.as_str())
    }

    fn get_expected_metadata_from_dir(dir: &str) -> Metadata {
        let fil_path = format!("{}{}/expected-metadata.json", TEST_TEXTURE_DIR, dir);
        let metadata_string = get_file_content(fil_path.as_str());
        let metadata: Metadata = serde_json::from_str(metadata_string.as_str()).unwrap();
        metadata
    }

    fn get_expected_from_dir(dir: &str) -> String {
        let fil_path = format!("{}{}/expected.html", TEST_TEXTURE_DIR, dir);
        let html = get_file_content(fil_path.as_str());
        let document = parse_html(html.as_str());
        let body = document.select_first("body").unwrap();
        body.as_node().inner_html()
    }

    #[test_resources("./test_textures/*")]
    fn run(resource: &str) {
        if resource.ends_with("DS_Store") {
            return;
        }
        test(resource);
    }

    #[test]
    fn debug_this() {
        let resource = match std::env::var("READABILITY_DEBUG_RESOURCE") {
            Ok(value) => value,
            Err(_) => return,
        };
        let source = get_source_from_dir(resource.as_str());
        let mut options = ExtractOptions::default();
        options.keep_classes = false;
        options.classes_to_preserve.insert(String::from("caption"));
        let result = extract(
            source.as_str(),
            "http://fakehost/test/page.html",
            options,
        );
        let actual = result.content.unwrap().inner_html();
        let expected = get_expected_from_dir(resource.as_str());
        std::fs::write("/tmp/readability-expected.html", expected.as_bytes()).unwrap();
        std::fs::write("/tmp/readability-actual.html", actual.as_bytes()).unwrap();
        println!("Wrote /tmp/readability-expected.html and /tmp/readability-actual.html");
    }
}
