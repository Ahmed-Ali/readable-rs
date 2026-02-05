//! Thin wrappers around [`super::logger::PerfLogger`] that compile to a no-op
//! in release builds (guarded by `cfg!(debug_assertions)`).
//!
//! | Macro | Forwards to |
//! |---|---|
//! | `start_span!` | `PerfLogger::start` |
//! | `end_span!` | `PerfLogger::end` |
//! | `add_point_to_span_str!` | `PerfLogger::check_point_str` |
//! | `add_point_to_span!` | `PerfLogger::check_point` (owned label) |
//! | `annotate_span_str!` | `PerfLogger::annotate_str` |
//! | `annotate_span!` | `PerfLogger::annotate` (owned label) |

macro_rules! start_span {
    ($logger:ident, $span_id:ident) => {
        if cfg!(debug_assertions) {
            $logger.start($span_id);
        }
    };
}

macro_rules! add_point_to_span_str {
    ($logger:ident, $span_id:ident, $point_str:expr) => {
        if cfg!(debug_assertions) {
            $logger.check_point_str($span_id, $point_str);
        }
    };
}

macro_rules! add_point_to_span {
    ($logger:ident, $span_id:ident, $point:expr) => {
        if cfg!(debug_assertions) {
            $logger.check_point($span_id, $point);
        }
    };
}

macro_rules! annotate_span_str {
    ($logger:ident, $span_id:ident, $annotation:expr) => {
        if cfg!(debug_assertions) {
            $logger.annotate_str($span_id, $annotation);
        }
    };
}

macro_rules! annotate_span {
    ($logger:ident, $span_id:ident, $annotation:expr) => {
        if cfg!(debug_assertions) {
            $logger.annotate($span_id, $annotation);
        }
    };
}

macro_rules! end_span {
    ($logger:ident, $span_id:ident) => {
        if cfg!(debug_assertions) {
            $logger.end($span_id);
        }
    };
}
