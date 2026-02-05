use crate::parser::NodeRef;
use html5ever::{LocalName, QualName};
use kuchikikiki::{Attributes, ElementData, NodeData};
use std::cell::RefCell;

/// DOM-navigation and element-manipulation helpers implemented on [`NodeRef`].
///
/// This trait is automatically in scope when you import from
/// [`crate::parser`] or [`crate::shared_utils`].
pub trait NodeExt {
    /// Return the local tag name of this node if it is an element (e.g.
    /// `"div"`, `"p"`), or `None` for text / comment / document nodes.
    fn element_name(&self) -> Option<&str>;

    /// Look up an attribute by name and return its value, or `None` if the
    /// attribute is absent or this is not an element node.
    fn attr_value(&self, name: &str) -> Option<String>;

    /// Collect the direct *element* children (skipping text and comment nodes)
    /// into a `Vec`.
    fn element_children(&self) -> Vec<NodeRef>;

    /// Return the first direct child that is an element, or `None`.
    fn first_element_child(&self) -> Option<NodeRef>;

    /// Walk forward through siblings until an element node is found, or
    /// return `None` if the end of the sibling list is reached.
    fn next_element_sibling(&self) -> Option<NodeRef>;

    /// Walk backward through siblings until an element node is found, or
    /// return `None` if the beginning of the sibling list is reached.
    fn previous_element_sibling(&self) -> Option<NodeRef>;

    /// Serialise the *children* of this node to an HTML string (the node's
    /// own open/close tags are **not** included).
    fn inner_html(&self) -> String;

    /// Create a new element with `tag_name`, copy all attributes and children
    /// from `self`, splice the new node into the tree in `self`'s position,
    /// and detach `self`.  Returns the new node.
    ///
    /// If `self` is not an element node it is returned unchanged.
    ///
    /// When the source tag is one of the legacy size-attribute elements
    /// (`table`, `th`, `td`, `hr`, `pre`) the `width` and `height`
    /// attributes are stripped from the copy.
    fn clone_and_rename_element(self, tag_name: &str) -> NodeRef;
}

/// Create a new, detached HTML element node with the given tag name and no
/// attributes or children.
///
/// # Examples
///
/// ```rust
/// use readable_rs::{new_html_element, NodeExt};
///
/// let div = new_html_element("div");
/// assert_eq!(div.element_name(), Some("div"));
/// ```
pub fn new_html_element(tag_name: &str) -> NodeRef {
    let name = QualName::new(None, html5ever::ns!(html), LocalName::from(tag_name));
    let attributes = Attributes {
        map: Default::default(),
    };
    NodeRef::new(NodeData::Element(ElementData {
        name,
        attributes: RefCell::new(attributes),
        template_contents: None,
    }))
}

impl NodeExt for NodeRef {
    fn element_name(&self) -> Option<&str> {
        self.as_element().map(|e| e.name.local.as_ref())
    }

    fn attr_value(&self, name: &str) -> Option<String> {
        self.as_element()
            .and_then(|e| e.attributes.borrow().get(name).map(|v| v.to_string()))
    }

    fn element_children(&self) -> Vec<NodeRef> {
        self.children()
            .filter(|c| c.as_element().is_some())
            .collect()
    }

    fn first_element_child(&self) -> Option<NodeRef> {
        self.children().find(|c| c.as_element().is_some())
    }

    fn next_element_sibling(&self) -> Option<NodeRef> {
        let mut sib = self.next_sibling();
        while let Some(node) = sib {
            if node.as_element().is_some() {
                return Some(node);
            }
            sib = node.next_sibling();
        }
        None
    }

    fn previous_element_sibling(&self) -> Option<NodeRef> {
        let mut sib = self.previous_sibling();
        while let Some(node) = sib {
            if node.as_element().is_some() {
                return Some(node);
            }
            sib = node.previous_sibling();
        }
        None
    }

    fn inner_html(&self) -> String {
        let mut out = String::new();
        for child in self.children() {
            out.push_str(&child.to_string());
        }
        out
    }

    fn clone_and_rename_element(self, tag_name: &str) -> NodeRef {
        if self.as_element().is_none() {
            return self;
        }
        let source_tag = self
            .element_name()
            .unwrap_or("")
            .to_lowercase();
        let e = self.as_element().unwrap();
        let name = QualName::new(None, html5ever::ns!(html), LocalName::from(tag_name));
        let new_node = NodeRef::new(NodeData::Element(ElementData {
            name,
            attributes: RefCell::new(Attributes {
                map: e.attributes.borrow().map.clone(),
            }),
            template_contents: e.template_contents.clone(),
        }));
        if matches!(source_tag.as_str(), "table" | "th" | "td" | "hr" | "pre") {
            if let Some(new_e) = new_node.as_element() {
                let mut attrs = new_e.attributes.borrow_mut();
                attrs.remove("width");
                attrs.remove("height");
            }
        }

        while self.first_child().is_some() {
            new_node.append(self.first_child().unwrap());
        }
        self.insert_before(new_node.clone());
        self.detach();

        new_node
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse_html;
    use std::panic::{catch_unwind, AssertUnwindSafe};

    #[test]
    fn clone_and_rename_element_non_element_does_not_panic() {
        let doc = parse_html("<div>text</div>");
        let div = doc.select_first("div").unwrap();
        let text_node = div.as_node().first_child().unwrap();
        let res = catch_unwind(AssertUnwindSafe(|| {
            let _ = text_node.clone().clone_and_rename_element("span");
        }));
        assert!(res.is_ok());
    }
}
