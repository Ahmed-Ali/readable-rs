# readable-rs

A native Rust implementation of the [Readability](https://github.com/mozilla/readability) algorithm for extracting the main readable content from HTML pages, stripping away navigation, ads, and other clutter.

This is a faithful port of Mozilla's Readability.js. The scoring algorithm, retry strategy, and heuristics match the upstream JavaScript library.

## Installation

```toml
[dependencies]
readable-rs = "0.1.2"
```

## Usage

```rust
use readable_rs::{extract, ExtractOptions};

let html = r#"
    <html><body>
        <article>
            <h1>My Article</h1>
            <p>The actual article content goes here. It needs to be long enough
            to clear the default character threshold before the algorithm will
            consider it a successful extraction.</p>
        </article>
        <nav><a href="/">Home</a><a href="/about">About</a></nav>
    </body></html>
"#;

let product = extract(html, "https://example.com/article", ExtractOptions::default());

if let Some(content) = &product.content {
    println!("Title:   {}", product.title);
    println!("Byline:  {}", product.by_line);
    println!("Content: {}", content.to_string());
}
```

## How it works

Given an HTML page, `extract` will:

1. Strip scripts, styles, comments, and navigation boilerplate
2. Detect and resolve lazy-loaded images and `<noscript>` fallbacks
3. Score candidate elements by content density (comma count, text length, link density, class/id heuristics)
4. Pick the highest-scoring subtree as the article body
5. Clean up the result: remove ads, empty nodes, presentation attributes, and rewrite relative URLs to absolute
6. Extract metadata (title, byline, site name, excerpt, publish date) from `<meta>` tags, JSON-LD, and heuristics

If the first pass yields fewer characters than `char_threshold` (default 500), the algorithm retries with progressively relaxed options.

## Configuration

All options live on [`ExtractOptions`](https://docs.rs/readable-rs/latest/readable_rs/struct.ExtractOptions.html):

| Field | Default | Description |
|-------|---------|-------------|
| `char_threshold` | 500 | Minimum character count for successful extraction |
| `strip_unlikelys` | true | Remove elements that look like navigation/ads |
| `clean_conditionally` | true | Remove low-density elements (few commas, high link ratio) |
| `weight_classes` | true | Use class/id names to adjust scoring |
| `remove_style_tags` | true | Strip `<style>` elements |
| `keep_classes` | true | Preserve CSS classes on output nodes |
| `ready_for_epub` | false | Apply stricter cleanup for EPUB compatibility |

## License

Apache-2.0 — see [LICENSE](LICENSE).
