use bitflags::bitflags;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::Deref;
use std::rc::Rc;
use std::time::Duration;
use std::time::Instant;

bitflags! {
    pub struct DebugLogsCategories: u32 {
        const NONE                     = 0;
        const CANDIDATES_PROGRESS      = 1 << 1;
        const DIV_CHILDREN_CHANGES     = 1 << 2;
        const CANDIDATE_GROUPS         = 1 << 3;
        const INITIAL_DOC              = 1 << 4;
        const RETRY_PARSING            = 1 << 5;
        const METHOD_DEBUG             = 1 << 6;
        const PREP_ARTICLE_METHODS     = 1 << 7;
        const DATA_TABLE_MARKING       = 1 << 8;
        const ALL                      = std::u32::MAX;
    }
}

/// Observer interface for performance-span events.  Implement this trait
/// and wrap it in a [`Listener`] to receive timing data from the extraction
/// pipeline.
///
/// All methods are called synchronously from the extraction code; keep
/// them lightweight.
pub trait PerfListener {
    /// Return whether this listener cares about the given span.  If `false`,
    /// none of the other callbacks will fire for that span.
    fn is_interested_in_span(&self, span_id: u64) -> bool;
    /// Called when a span begins.
    fn on_span_start(&self, span_id: u64, start_time: Instant);
    /// Called at each checkpoint within a span, with the wall-clock duration
    /// since the previous checkpoint (or span start).
    fn on_check_point(
        &self,
        span_id: u64,
        point_time: Instant,
        duration_since_last_checkpoint: Duration,
        label: &str,
    );
    /// Called when a free-text annotation is attached to a span.
    fn on_annotate(&self, span_id: u64, annotation: &str);
    /// Called when a span ends, with its total duration.
    fn on_span_end(&self, span_id: u64, span_duration: Duration);
}

/// A clonable, reference-counted wrapper around a [`PerfListener`].
/// Clone is cheap (just an `Rc` bump); the underlying listener is shared.
#[derive(Clone)]
pub struct Listener {
    inner_impl: Rc<dyn PerfListener>,
}

impl Listener {
    /// Wrap a [`PerfListener`] implementation for use with [`PerfLogger`].
    pub fn new(listner: Rc<dyn PerfListener>) -> Listener {
        Listener {
            inner_impl: listner,
        }
    }
}

impl Deref for Listener {
    type Target = dyn PerfListener;
    fn deref(&self) -> &Self::Target {
        &*self.inner_impl
    }
}

struct PerfCheckPoint {
    pub label: String,
    pub time: Instant,
}

struct PerfEvent {
    pub span_id: u64,
    pub start_time: Instant,
    pub points: Vec<PerfCheckPoint>,
    pub annotations: Vec<String>,
    pub listeners: Vec<Listener>,
}

impl PerfEvent {
    pub fn point(&mut self, point: PerfCheckPoint) {
        let duration_since_last_checkpoint = if self.points.is_empty() {
            point.time.duration_since(self.start_time)
        } else {
            point.time.duration_since(self.points.last().unwrap().time)
        };
        self.listeners.iter().for_each(|l| {
            l.on_check_point(
                self.span_id,
                point.time,
                duration_since_last_checkpoint,
                point.label.as_str(),
            )
        });
        self.points.push(point);
    }

    pub fn annotate(&mut self, annotation: String) {
        self.listeners.iter().for_each(|l| {
            l.on_annotate(self.span_id, annotation.as_str());
        });
        self.annotations.push(annotation);
    }
}

/// Tracks in-flight performance spans and fans events out to registered
/// [`Listener`]s.
///
/// **Not `Send` or `Sync`** — the internal event map uses `RefCell`.  Each
/// extraction call creates its own `PerfLogger`.
pub struct PerfLogger {
    events: RefCell<HashMap<u64, PerfEvent>>,
    listeners: Vec<Listener>,
}

impl PerfLogger {
    /// Create a new logger with the given set of listeners.  Pass an empty
    /// `Vec` to disable all perf logging (zero overhead in that case).
    pub fn new(listeners: Vec<Listener>) -> PerfLogger {
        PerfLogger {
            events: RefCell::new(HashMap::new()),
            listeners,
        }
    }

    /// Begin a new span identified by `span_id`.  Only listeners that
    /// return `true` from [`PerfListener::is_interested_in_span`] are
    /// notified and stored.
    pub fn start(&self, span_id: u64) {
        let event_listeners = self
            .listeners
            .iter()
            .filter(|l| l.is_interested_in_span(span_id))
            .cloned()
            .collect::<Vec<_>>();
        if !event_listeners.is_empty() {
            let start_time = Instant::now();
            event_listeners
                .iter()
                .for_each(|l| l.on_span_start(span_id, start_time));
            let event = PerfEvent {
                span_id,
                start_time,
                points: vec![],
                annotations: vec![],
                listeners: event_listeners,
            };
            self.events.borrow_mut().insert(span_id, event);
        }
    }

    /// Record a checkpoint with a `&str` label inside the given span.
    pub fn check_point_str(&self, span_id: u64, label: &str) {
        if let Some(event) = self.events.borrow_mut().get_mut(&span_id) {
            let time = Instant::now();
            let point = PerfCheckPoint {
                label: String::from(label),
                time,
            };

            event.point(point);
        }
    }

    /// Record a checkpoint with an owned `String` label inside the given span.
    pub fn check_point(&self, span_id: u64, label: String) {
        if let Some(event) = self.events.borrow_mut().get_mut(&span_id) {
            let time = Instant::now();
            let point = PerfCheckPoint { label, time };
            event.point(point);
        }
    }

    /// Attach a free-text annotation (`&str`) to the given span.
    pub fn annotate_str(&self, span_id: u64, annotation: &str) {
        if let Some(event) = self.events.borrow_mut().get_mut(&span_id) {
            event.annotate(String::from(annotation));
        }
    }

    /// Attach a free-text annotation (owned `String`) to the given span.
    pub fn annotate(&self, span_id: u64, annotation: String) {
        if let Some(event) = self.events.borrow_mut().get_mut(&span_id) {
            event.annotate(annotation);
        }
    }

    /// End the span, notify listeners with the total duration, and remove
    /// it from the active-events map.
    pub fn end(&self, span_id: u64) {
        if let Some(event) = self.events.borrow().get(&span_id) {
            let now = Instant::now();
            let duration = now.duration_since(event.start_time);
            event.listeners.iter().for_each(|l| {
                l.on_span_end(span_id, duration);
            });
        }
        self.events.borrow_mut().remove(&span_id);
    }
}
