pub mod logger;

pub mod logging_defs;
#[macro_use]
pub mod macros;

use logger::PerfListener;
use logging_defs::*;
use std::time::Duration;
use std::time::Instant;

/// A [`PerfListener`] that prints every span event to stdout.
/// Useful during development; note that [`is_interested_in_span`][PerfListener::is_interested_in_span]
/// currently returns `false`, so no events are emitted unless you
/// implement your own listener.
pub struct PerfConsoleListener;

impl PerfListener for PerfConsoleListener {
    fn is_interested_in_span(&self, _span_id: u64) -> bool {
        // _span_id == EXTRACT
        false
    }

    fn on_span_start(&self, span_id: u64, _start_time: Instant) {
        println!("Start of span: {}", name(span_id));
    }

    fn on_check_point(
        &self,
        span_id: u64,
        _point_time: Instant,
        duration_since_last_checkpoint: Duration,
        point_label: &str,
    ) {
        println!(
            "Span: \"{}\" point: \"{}\": {} seconds",
            name(span_id),
            point_label,
            duration_since_last_checkpoint.as_secs_f64()
        );
    }

    fn on_annotate(&self, span_id: u64, annotation: &str) {
        println!("Span: \"{}\" annotation: \"{}\"", name(span_id), annotation);
    }

    fn on_span_end(&self, span_id: u64, span_duration: Duration) {
        println!(
            "Span ended: \"{}\": {} seconds",
            name(span_id),
            span_duration.as_secs_f64()
        );
    }
}
