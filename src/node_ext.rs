use crate::parser::NodeRef;
use std::collections::HashMap;

#[derive(Clone, Copy, Debug, Default)]
struct NodeMeta {
    readability_score: Option<f64>,
    is_readability_data_table: bool,
}

/// An external store that maps DOM nodes to readability metadata without
/// mutating the nodes themselves.
///
/// [`NodeRef`] values are reference-counted and cannot hold arbitrary
/// side-channel data, so the scorer keeps a `HashMap` keyed by the
/// underlying pointer address.  Each call to [`crate::extract`] produces
/// its own `NodeScoreStore`; scores are **not** shared across extractions.
///
/// # Examples
///
/// ```rust
/// use readable_rs::parser::parse_html;
/// use readable_rs::NodeScoreStore;
///
/// let doc = parse_html("<div><p>hello</p></div>");
/// let store = NodeScoreStore::default();
/// // store is empty — scores are populated internally during extraction.
/// ```
#[derive(Default, Debug, Clone)]
pub struct NodeScoreStore {
    map: HashMap<usize, NodeMeta>,
}

/// Derive a stable key for a [`NodeRef`] by taking the address of the
/// inner `Node` value.  Two `NodeRef`s that point to the same tree node
/// will produce the same key.
fn node_key(node: &NodeRef) -> usize {
    let ptr: *const _ = &**node;
    ptr as usize
}

/// Extension trait that attaches readability scoring and table-classification
/// metadata to a [`NodeRef`] via an external [`NodeScoreStore`].
///
/// Implemented for [`NodeRef`].  All methods require an explicit store
/// argument so that the caller controls lifetime and isolation.
pub trait NodeScoreExt {
    /// Return the readability score previously assigned to this node, or
    /// `None` if it has not been scored yet.
    fn readability_score(&self, store: &NodeScoreStore) -> Option<f64>;

    /// Add `offset` to this node's current score (treating an absent score
    /// as `0.0`).
    fn offset_readability_score(&self, store: &mut NodeScoreStore, offset: f64);

    /// Overwrite this node's readability score.  Pass `None` to clear it.
    fn set_readability_score(&self, store: &mut NodeScoreStore, value: Option<f64>);

    /// Return whether this node has been classified as a *data* table
    /// (as opposed to a layout table).  Defaults to `false`.
    fn is_readability_data_table(&self, store: &NodeScoreStore) -> bool;

    /// Mark (or unmark) this node as a data table.  Data tables are
    /// protected from removal during the "clean conditionally" pass.
    fn set_readability_data_table(&self, store: &mut NodeScoreStore, is_readability_data_table: bool);
}

impl NodeScoreExt for NodeRef {
    fn readability_score(&self, store: &NodeScoreStore) -> Option<f64> {
        let key = node_key(self);
        store.map.get(&key).and_then(|m| m.readability_score)
    }

    fn offset_readability_score(&self, store: &mut NodeScoreStore, offset: f64) {
        let key = node_key(self);
        let entry = store.map.entry(key).or_default();
        entry.readability_score = Some(entry.readability_score.unwrap_or(0.0) + offset);
    }

    fn set_readability_score(&self, store: &mut NodeScoreStore, value: Option<f64>) {
        let key = node_key(self);
        let entry = store.map.entry(key).or_default();
        entry.readability_score = value;
    }

    fn is_readability_data_table(&self, store: &NodeScoreStore) -> bool {
        let key = node_key(self);
        store
            .map
            .get(&key)
            .map(|m| m.is_readability_data_table)
            .unwrap_or(false)
    }

    fn set_readability_data_table(&self, store: &mut NodeScoreStore, is_readability_data_table: bool) {
        let key = node_key(self);
        let entry = store.map.entry(key).or_default();
        entry.is_readability_data_table = is_readability_data_table;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse_html;

    #[test]
    fn store_isolated_per_instance() {
        let doc = parse_html("<div><p>Hello</p></div>");
        let p = doc.select_first("p").unwrap().as_node().clone();
        let mut store_a = NodeScoreStore::default();
        let mut store_b = NodeScoreStore::default();

        p.set_readability_score(&mut store_a, Some(10.0));
        assert_eq!(p.readability_score(&store_a), Some(10.0));
        assert_eq!(p.readability_score(&store_b), None);

        p.set_readability_score(&mut store_b, Some(5.0));
        assert_eq!(p.readability_score(&store_a), Some(10.0));
        assert_eq!(p.readability_score(&store_b), Some(5.0));
    }
}
