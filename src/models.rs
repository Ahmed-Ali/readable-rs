use crate::parser::NodeRef;
use crate::node_ext::NodeScoreStore;
use std::collections::HashSet;

/// Metadata extracted from a page's `<meta>` tags, JSON-LD, and heuristic
/// byline / title detection.  Used internally during extraction; the fields
/// are surfaced on [`Product`].
#[derive(Debug, Clone)]
pub struct Metadata {
    /// The page title, cleaned and de-duplicated against site-name suffixes.
    pub title: String,
    /// The article author / byline string, if one could be detected.
    pub by_line: String,
    /// The site name (e.g. from `og:site_name`).
    pub sitename: String,
    /// A short excerpt / description (e.g. from `og:description`).
    pub excerpt: String,
    /// An ISO-8601 publish timestamp, if present in the page metadata.
    pub published_time: String,
}

/// The output of [`crate::extract`].  Contains the extracted article content
/// as a DOM subtree together with any metadata that was found.
#[derive(Debug, Clone, Default)]
pub struct Product {
    /// The cleaned page / article title.
    pub title: String,
    /// The extracted content subtree, or `None` if no article content could
    /// be identified.  Serialise to HTML with `.as_ref().map(|n| n.to_string())`.
    pub content: Option<NodeRef>,
    /// The author / byline string, if detected.
    pub by_line: String,
    /// The dominant text direction (`"ltr"` or `"rtl"`), inferred from
    /// ancestor `dir` attributes of the top candidate.  Empty string if unknown.
    pub dir: String,
    /// The site name from page metadata.
    pub sitename: String,
    /// A short excerpt / description from page metadata.
    pub excerpt: String,
    /// An ISO-8601 publish timestamp from page metadata.
    pub published_time: String,
    /// The per-node score store produced during scoring.  Useful for
    /// introspection / debugging; not needed for normal consumers.
    pub score_store: NodeScoreStore,
}

/// Knobs that control the behaviour of the extraction algorithm.
///
/// All fields have sensible defaults via [`Default`]; start there and only
/// override what you need.
///
/// # Examples
///
/// ```rust
/// use readable_rs::ExtractOptions;
///
/// let mut opts = ExtractOptions::default();
/// opts.char_threshold = 200;   // accept shorter articles
/// opts.remove_style_tags = false; // keep <style> elements
/// ```
#[derive(Debug, Clone)]
pub struct ExtractOptions {
    /// Enable extra `eprintln!` tracing inside the algorithm (gated behind
    /// `debug_assertions` in release builds).
    pub debug: bool,
    /// Strip all `<style>` elements from the document before extraction.
    pub remove_style_tags: bool,
    /// When `true`, apply additional cleanup passes that produce output
    /// suitable for embedding in an EPUB (e.g. stricter image handling).
    pub ready_for_epub: bool,
    /// Remove elements whose class / id / role strongly suggest they are
    /// navigation, ads, or other non-content.  Disabling this is one of the
    /// retry strategies when the first pass yields too little text.
    pub strip_unlikelys: bool,
    /// Use class-name / id heuristics (positive/negative word lists) to
    /// adjust candidate scores.  Disabling is another retry strategy.
    pub weight_classes: bool,
    /// When `true`, preserve CSS class attributes on the output nodes
    /// (subject to [`classes_to_preserve`][Self::classes_to_preserve]).
    /// When `false`, all class attributes are stripped.
    pub keep_classes: bool,
    /// The set of class names that are *always* kept even when
    /// [`keep_classes`][Self::keep_classes] is `false`.  Readability's own
    /// marker classes (e.g. `"page"`) are added automatically.
    pub classes_to_preserve: HashSet<String>,
    /// Apply the "clean conditionally" pass, which removes elements with
    /// low content density (few commas, high link density, etc.).
    /// Disabling is the third retry strategy.
    pub clean_conditionally: bool,
    /// Maximum number of scoring candidates to evaluate.  `0` means no limit.
    pub max_elements_to_parse: u16,
    /// How many top-scoring candidate nodes to retain before picking the
    /// winner.  Higher values make the algorithm slightly more robust against
    /// mis-scored nodes.
    pub n_top_candidates: u16,
    /// Minimum character count the extracted content must reach before it is
    /// accepted.  If the first pass falls short, the algorithm retries with
    /// progressively relaxed options.
    pub char_threshold: u16,
    /// An additive modifier applied to the link-density thresholds used in
    /// the "clean conditionally" pass.  Positive values make the filter
    /// more lenient (tolerate higher link density).
    pub link_density_modifier: f64,
}

impl Default for ExtractOptions {
    fn default() -> ExtractOptions {
        ExtractOptions {
            debug: false,
            remove_style_tags: true,
            ready_for_epub: false,
            strip_unlikelys: true,
            weight_classes: true,
            keep_classes: true,
            classes_to_preserve: HashSet::new(),
            clean_conditionally: true,
            max_elements_to_parse: 0,
            n_top_candidates: 5,
            char_threshold: 500,
            link_density_modifier: 0.0,
        }
    }
}
