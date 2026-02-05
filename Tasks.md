# Readability-RS Audit Tasks

## High Severity

- [x] **1a** — Hoist all per-call / in-loop regex compilations into `lazy_static!` statics
  - `unescape_html_entities` (utils.rs:684, 697)
  - `fix_lazy_images` (utils.rs:902, 942, 947)
  - `unwrap_noscript_images` (utils.rs:1008, 1055)
  - `should_append_sibling` (article/utils.rs:209)
  - `get_metadata` (extractor.rs:340-348)
  - `get_json_ld_metadata` (extractor.rs:110)
  - `get_clean_title_from_title_tag` (extractor.rs:489-553)

- [x] **4c** — Fix broken `B64_DATA_URL` and `SRCSET_URL` regexes (double-escaped `\\s` / `\\S` / `\\d` in raw strings)

## Medium Severity

- [x] **1c** — Reduce repeated `select_descendants` calls in `is_safe_to_remove_node` (article/utils.rs)
- [x] **2a** — Unify duplicate `is_element_without_content` / `is_element_node_without_content`
- [x] **3h** — Remove unnecessary `RefCell` wrapping on `ContentParser` fields; use mutable locals in `parse()`
- [x] **3m** — Convert recursive `clean_classes` to an iterative tree walk to avoid stack overflow on deep/wide trees
- [x] **3n** — Guard `READABILITY_STAGE_DUMP` env-var block behind the `debug` option flag instead of running unconditionally

## Low Severity

- [x] **1d** — Use `HashSet` for `tokens_a` in `text_similarity` to fix O(n*m) containment check
- [x] **2b** — Remove dead `AD_WORDS_REGEX` and `LOADING_WORDS_REGEX` statics
- [x] **2c** — Remove dead `ARTICLE_TITLE_KEYS` static
- [x] **2d** — Remove redundant `use std::iter::FromIterator` imports (utils.rs, article.rs, cleaner.rs)
- [x] **3a** — Rename `ReadabilityPorcessor` → `ReadabilityProcessor`
- [x] **3b** — Rename `char_threashold` → `char_threshold` (public field)
- [x] **3c** — Rename `artcile_title` → `article_title` parameter
- [x] **3d** — Rename `DEFAULT_MAX_ANCESTROS_DEPTH` → `DEFAULT_MAX_ANCESTORS_DEPTH`
- [x] **3e** — Rename `PerfConsoleListner` → `PerfConsoleListener`
- [x] **3f** — Fix typos in test names (`wieght` → `weight`, `negiatives` → `negatives`)
- [x] **3g** — Change `extract(doc_uri: String)` to `extract(doc_uri: &str)`
- [x] **3i** — Fix `concate_optionals` to not emit a leading separator when `l` is `None`; rename to `concat_optionals`
- [x] **3j** — Rewrite `next_element` to use `while let` and eliminate redundant clones
- [x] **3k** — Clean up parent-walk loop in `get_next_node` (utils.rs:264)
- [x] **3l** — Fix double-clone in `get_node_ancestors` (utils.rs:479)
- [x] **4a** — Fix duplicate `"pick_top_n_candidates_begin"` label in scorer.rs:290
- [x] **4b** — Fix misleading span-ID scheme in `logging_defs.rs` and update comment
- [x] **4d** — Fix off-by-one in `has_ancestor_tag_with_predicate` depth check
- [x] **2g** — Remove redundant `count_elements` definitions; make a shared test helper
- [x] **5a** — Replace `lazy_static` + `#[macro_use] extern crate` with `once_cell` / `std::sync::OnceLock`
