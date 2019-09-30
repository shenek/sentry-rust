//! Adds support for automatic breadcrumb capturing from logs with `slog`.
//!
//! **Feature:** `with_slog`
//!
//! # Configuration
//!
//! In the most trivial version you could proceed like this:
//!
//! ```no_run
//! # extern crate slog;
//!
//! let drain = slog::Discard;
//! let wrapped_drain = sentry::integrations::slog::wrap_drain(drain, Default::default(), None);
//! let root = slog::Logger::root(drain, slog::o!());
//!
//! slog::warn!(root, "Log for sentry")
//! ```
use slog::{Drain, Serializer, KV};
use std::{collections::BTreeMap, fmt, sync::Arc};

use crate::{
    hub::Hub,
    protocol::{Breadcrumb, Event},
    Level,
};

// Serializer which stores the serde_json values in BTreeMap
#[derive(Default)]
struct StoringSerializer {
    result: BTreeMap<String, serde_json::Value>,
}

impl StoringSerializer {
    #[allow(missing_docs)]
    fn emit_serde_json_value(&mut self, key: slog::Key, val: serde_json::Value) -> slog::Result {
        self.result.insert(key.to_string(), val);
        Ok(())
    }

    #[allow(missing_docs)]
    fn emit_serde_json_null(&mut self, key: slog::Key) -> slog::Result {
        self.emit_serde_json_value(key, serde_json::Value::Null)
    }

    #[allow(missing_docs)]
    fn emit_serde_json_bool(&mut self, key: slog::Key, val: bool) -> slog::Result {
        self.emit_serde_json_value(key, serde_json::Value::Bool(val))
    }

    #[allow(missing_docs)]
    fn emit_serde_json_number<V>(&mut self, key: slog::Key, value: V) -> slog::Result
    where
        serde_json::Number: From<V>,
    {
        let num = serde_json::Number::from(value);
        self.emit_serde_json_value(key, serde_json::Value::Number(num))
    }

    #[allow(missing_docs)]
    fn emit_serde_json_string(&mut self, key: slog::Key, val: String) -> slog::Result {
        self.emit_serde_json_value(key, serde_json::Value::String(val))
    }
}

macro_rules! impl_number {
    ( $type:ty => $function_name:ident ) => {
        #[allow(missing_docs)]
        fn $function_name(&mut self, key: slog::Key, val: $type) -> slog::Result {
            self.emit_serde_json_number(key, val)
        }
    };
}

impl Serializer for StoringSerializer {
    #[allow(missing_docs)]
    fn emit_bool(&mut self, key: slog::Key, val: bool) -> slog::Result {
        self.emit_serde_json_bool(key, val)
    }

    #[allow(missing_docs)]
    fn emit_unit(&mut self, key: slog::Key) -> slog::Result {
        self.emit_serde_json_null(key)
    }

    #[allow(missing_docs)]
    fn emit_none(&mut self, key: slog::Key) -> slog::Result {
        self.emit_serde_json_null(key)
    }

    #[allow(missing_docs)]
    fn emit_char(&mut self, key: slog::Key, val: char) -> slog::Result {
        self.emit_serde_json_string(key, val.to_string())
    }

    #[allow(missing_docs)]
    fn emit_str(&mut self, key: slog::Key, val: &str) -> slog::Result {
        self.emit_serde_json_string(key, val.to_string())
    }

    #[allow(missing_docs)]
    fn emit_f64(&mut self, key: slog::Key, val: f64) -> slog::Result {
        if let Some(num) = serde_json::Number::from_f64(val) {
            self.emit_serde_json_value(key, serde_json::Value::Number(num))
        } else {
            self.emit_serde_json_null(key)
        }
    }

    impl_number!(u8 => emit_u8);
    impl_number!(i8 => emit_i8);
    impl_number!(u16 => emit_u16);
    impl_number!(i16 => emit_i16);
    impl_number!(u32 => emit_u32);
    impl_number!(i32 => emit_i32);
    impl_number!(u64 => emit_u64);
    impl_number!(i64 => emit_i64);

    // u128 and i128 should be implemented in serde_json 1.0.40
    // impl_number!(u128 => emit_u128);
    // impl_number!(i128 => emit_i128);

    #[allow(missing_docs)]
    fn emit_arguments(&mut self, _: slog::Key, _: &fmt::Arguments) -> slog::Result {
        Ok(())
    }
}

/// Converts `slog::Level` to `Level`
fn into_sentry_level(slog_level: slog::Level) -> Level {
    match slog_level {
        slog::Level::Trace | slog::Level::Debug => Level::Debug,
        slog::Level::Info => Level::Info,
        slog::Level::Warning => Level::Warning,
        slog::Level::Error | slog::Level::Critical => Level::Error,
    }
}

/// Options for the slog configuration
#[derive(Debug, Copy, Clone)]
pub struct Options {
    /// Level since when the breadcrumbs are created
    breadcrumb_level: Option<slog::Level>,
    /// Level since when the events are sent
    event_level: Option<slog::Level>,
}

impl Options {
    /// Creates new slog integration options
    pub fn new(breadcrumb_level: Option<slog::Level>, event_level: Option<slog::Level>) -> Self {
        Self {
            breadcrumb_level,
            event_level,
        }
    }
}

impl Default for Options {
    fn default() -> Self {
        Self {
            breadcrumb_level: Some(slog::Level::Info),
            event_level: Some(slog::Level::Warning),
        }
    }
}

/// Prepare data for sentry
pub fn process_record(
    record: &slog::Record,
    kv: Option<&dyn slog::KV>,
    options: Options,
    hub: Option<Arc<Hub>>,
) {
    let mut structured_data = BTreeMap::new();

    // Append logger KVs
    if let Some(kv_data) = kv {
        let mut storing_serializer_1 = StoringSerializer::default();
        if kv_data.serialize(record, &mut storing_serializer_1).is_ok() {
            structured_data.append(&mut storing_serializer_1.result)
        }
    }

    // Append record KVs
    let mut storing_serializer_2 = StoringSerializer::default();
    if record
        .kv()
        .serialize(record, &mut storing_serializer_2)
        .is_ok()
    {
        structured_data.append(&mut storing_serializer_2.result)
    }

    // get hub
    let hub = if let Some(hub) = hub {
        hub
    } else {
        Hub::current()
    };

    if options.event_level.is_some() && record.level() <= options.event_level.unwrap() {
        // Capture an event
        let event = Event {
            message: Some(record.msg().to_string()),
            level: into_sentry_level(record.level()),
            ..Event::default()
        };

        hub.with_scope(
            |scope| {
                for (key, value) in structured_data {
                    scope.set_extra(&key, value);
                }
            },
            || {
                hub.capture_event(event);
            },
        );
    } else if options.breadcrumb_level.is_some()
        && record.level() <= options.breadcrumb_level.unwrap()
    {
        // Create a breadcrumb
        let breadcrumb = Breadcrumb {
            message: Some(record.msg().to_string()),
            level: into_sentry_level(record.level()),
            data: structured_data,
            ..Breadcrumb::default()
        };

        hub.add_breadcrumb(breadcrumb);
    }
}

/// Wrapped drain for sentry logging
#[derive(Debug, Clone)]
pub struct WrappedDrain<D>
where
    D: Drain,
{
    drain: D,
    options: Options,
    hub: Option<Arc<Hub>>,
}

impl<D> WrappedDrain<D>
where
    D: Drain,
{
    /// Creates a new wrapped Drain
    fn new(drain: D, options: Options, hub: Option<Arc<Hub>>) -> Self {
        Self {
            drain,
            options,
            hub,
        }
    }

    /// Processes slog record
    fn process_record(&self, record: &slog::Record, kv: &slog::OwnedKVList) {
        process_record(record, Some(kv), self.options, self.hub.clone());
    }
}

impl<D> Drain for WrappedDrain<D>
where
    D: Drain,
{
    type Ok = D::Ok;
    type Err = D::Err;

    fn log(
        &self,
        record: &slog::Record,
        values: &slog::OwnedKVList,
    ) -> Result<Self::Ok, Self::Err> {
        self.process_record(record, values);
        self.drain.log(record, values)
    }
}

impl<D> std::ops::Deref for WrappedDrain<D>
where
    D: Drain,
{
    type Target = D;

    fn deref(&self) -> &Self::Target {
        &self.drain
    }
}

/// Wraps `slog::Drain`
pub fn wrap_drain<D>(drain: D, options: Options, hub: Option<Arc<Hub>>) -> WrappedDrain<D>
where
    D: slog::Drain,
{
    WrappedDrain::new(drain, options, hub)
}
