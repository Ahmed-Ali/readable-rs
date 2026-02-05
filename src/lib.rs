//! A Rust port of Mozilla's [Readability](https://github.com/nicolo-ribaudo/readability) algorithm
//! for extracting the main article content from an HTML page.
//!
//! ## Quick start
//!
//! ```rust
//! use readable_rs::{extract, ExtractOptions};
//!
//! let html = "<html><body><article><p>The actual article text goes here.</p></article></body></html>";
//! let product = extract(html, "https://example.com/article", ExtractOptions::default());
//!
//! // product.content holds the extracted DOM (or None if nothing was found)
//! // product.title, product.by_line, product.sitename, etc. hold metadata
//! ```
//!
//! ## Module layout
//!
//! * **Top level** – [`extract`] is the single entry-point.  [`Product`] and
//!   [`ExtractOptions`] are the main public types.
//! * [`parser`] – thin wrappers around the underlying HTML parser ([`parser::NodeRef`],
//!   [`parser::parse_html`]).
//! * [`shared_utils`] – a curated set of DOM helpers useful when post-processing
//!   the extracted content (URL resolution, text normalisation, etc.).
//! * [`NodeExt`] / [`NodeScoreStore`] – the trait and store that the scorer uses
//!   to attach readability metadata to DOM nodes without modifying the nodes themselves.

macro_rules! d {
    ($code:block) => {
        if cfg!(debug_assertions) {
            $code
        }
    };
}

#[macro_use]
mod logging;
mod extractor;

mod models;
mod node_ext;
mod node_utils;
mod utils;

pub use models::{Product, ExtractOptions};
pub use node_ext::NodeScoreStore;
pub use node_utils::{new_html_element, NodeExt};

/// Convenience re-exports of DOM helpers for post-processing extracted content.
///
/// These are a stable, curated subset of the internal utility library.
pub mod shared_utils {
    pub use crate::utils::{
        apply, contains_single_tag_in_element, move_children, normalize_text,
        replace_relative_urls_with_absolute, word_count,
    };
}

/// Thin wrappers around the underlying HTML parser.
///
/// [`NodeRef`] is the reference-counted DOM node type used throughout the crate.
/// [`parse_html`] parses a complete HTML document into a [`NodeRef`] tree.
pub mod parser {
    use kuchikikiki::traits::TendrilSink;
    pub use kuchikikiki::{Attributes, NodeRef};
    pub use crate::node_utils::{new_html_element, NodeExt};

    /// Parse an HTML string into a [`NodeRef`] document tree.
    ///
    /// The parser follows the HTML5 specification; an implicit `<html>`, `<head>`,
    /// and `<body>` are synthesised when missing.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use readable_rs::parser::parse_html;
    ///
    /// let doc = parse_html("<div><p>hello</p></div>");
    /// assert!(doc.select_first("p").is_ok());
    /// ```
    pub fn parse_html(html: &str) -> NodeRef {
        kuchikikiki::parse_html().one(html)
    }
}

/// Extract the main article content from an HTML page.
///
/// This is the primary entry-point of the crate.  It implements the Readability
/// algorithm: scoring candidate nodes by content density, pruning navigation /
/// boilerplate, and returning the best content subtree along with any metadata
/// (title, byline, etc.) that could be extracted.
///
/// # Arguments
///
/// * `html_str` – the raw HTML source of the page.
/// * `doc_uri` – the URL the page was fetched from.  Used to resolve relative
///   URLs in `<a href>`, `<img src>`, `srcset`, etc.
/// * `options` – tuning knobs for the extraction algorithm.  [`ExtractOptions::default()`]
///   is a sensible starting point.
///
/// # Returns
///
/// A [`Product`] whose `content` field is `Some` if article content was found,
/// or `None` if the page did not contain extractable content.
///
/// # Examples
///
/// ```rust
/// use readable_rs::{extract, ExtractOptions};
///
/// let html = "<html><body><p>Short.</p></body></html>";
/// let product = extract(html, "https://example.com", ExtractOptions::default());
/// // product.content may be None — the paragraph is below the default char_threshold.
/// ```
pub fn extract(html_str: &str, doc_uri: &str, options: ExtractOptions) -> Product {
    let processor = extractor::Extractor::new(html_str, doc_uri.to_string(), options);
    processor.extract()
}
