/// Span IDs for performance-logging events.  Each ID must be unique;
/// simply increment when adding a new span.
pub const EXTRACT: u64 = 1;
pub const PREP_DOC: u64 = 2;
pub const GET_METADATA: u64 = 3;
pub const PARSE_CONTENT: u64 = 4;
pub const CONTENT_CLEANUP: u64 = 5;
pub const CONTENT_SCORING: u64 = 6;
pub const CS_GET_TOP_CANDIDATES: u64 = 7;
pub const CS_CALC_READABILITY_SCORE_WITH_LINK_DENSITY: u64 = 8;
pub const GET_LINK_DENSITY: u64 = 9;
pub const NORMALIZE_AND_COUNT_CHARS: u64 = 10;
pub const NORMALIZE_NODE_TEXT: u64 = 11;
pub const INITIALIZE_READABILITY_SCORE: u64 = 12;

pub fn name(span_id: u64) -> &'static str {
    match span_id {
        EXTRACT => "EXTRACT",
        PREP_DOC => "PREP_DOC",
        GET_METADATA => "GET_METADATA",
        PARSE_CONTENT => "PARSE_CONTENT",
        CONTENT_CLEANUP => "CONTENT_CLEANUP",
        CONTENT_SCORING => "CONTENT_SCORING",
        CS_GET_TOP_CANDIDATES => "CS_GET_TOP_CANDIDATES",
        CS_CALC_READABILITY_SCORE_WITH_LINK_DENSITY => {
            "CS_CALC_READABILITY_SCORE_WITH_LINK_DENSITY"
        }
        GET_LINK_DENSITY => "GET_LINK_DENSITY",
        NORMALIZE_AND_COUNT_CHARS => "NORMALIZE_AND_COUNT_CHARS",
        NORMALIZE_NODE_TEXT => "NORMALIZE_NODE_TEXT",
        INITIALIZE_READABILITY_SCORE => "INITIALIZE_READABILITY_SCORE",
        _ => panic!(
            "Calling logging::logging_defs::name with unknown span_id: {}",
            span_id
        ),
    }
}
