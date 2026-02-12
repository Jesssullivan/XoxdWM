//! Clock abstraction for deterministic testing.
//!
//! Production code uses `SystemClock` (real time).
//! Tests use `TestClock` with manual time advancement.

use std::sync::Mutex;
use std::time::{Duration, Instant, SystemTime};

/// Trait abstracting time sources for testability.
pub trait Clock: Send + Sync {
    /// Returns the current monotonic instant.
    fn now(&self) -> Instant;

    /// Returns the current wall-clock time as milliseconds
    /// since UNIX epoch.
    fn unix_millis(&self) -> i64;
}

/// Production clock using real system time.
#[derive(Debug, Clone, Copy)]
pub struct SystemClock;

impl Clock for SystemClock {
    fn now(&self) -> Instant {
        Instant::now()
    }

    fn unix_millis(&self) -> i64 {
        SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .map(|d| d.as_millis() as i64)
            .unwrap_or(0)
    }
}

/// Test clock with manually controlled time.
pub struct TestClock {
    instant: Mutex<Instant>,
    unix_ms: Mutex<i64>,
}

impl TestClock {
    /// Create a test clock starting at the current real time.
    pub fn new() -> Self {
        Self {
            instant: Mutex::new(Instant::now()),
            unix_ms: Mutex::new(
                SystemTime::now()
                    .duration_since(SystemTime::UNIX_EPOCH)
                    .map(|d| d.as_millis() as i64)
                    .unwrap_or(0),
            ),
        }
    }

    /// Advance time by the given duration.
    pub fn advance(&self, duration: Duration) {
        let mut inst = self.instant.lock().unwrap();
        *inst += duration;
        let mut ms = self.unix_ms.lock().unwrap();
        *ms += duration.as_millis() as i64;
    }

    /// Set the UNIX millisecond timestamp explicitly.
    pub fn set_unix_millis(&self, ms: i64) {
        *self.unix_ms.lock().unwrap() = ms;
    }
}

impl Clock for TestClock {
    fn now(&self) -> Instant {
        *self.instant.lock().unwrap()
    }

    fn unix_millis(&self) -> i64 {
        *self.unix_ms.lock().unwrap()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;

    #[test]
    fn test_system_clock_returns_time() {
        let clock = SystemClock;
        let now = clock.now();
        let ms = clock.unix_millis();
        assert!(ms > 0);
        // Monotonic: a second call should be >= first
        assert!(clock.now() >= now);
    }

    #[test]
    fn test_test_clock_advance() {
        let clock = TestClock::new();
        let t0 = clock.now();
        let ms0 = clock.unix_millis();

        clock.advance(Duration::from_secs(5));

        let t1 = clock.now();
        let ms1 = clock.unix_millis();

        assert_eq!(t1 - t0, Duration::from_secs(5));
        assert_eq!(ms1 - ms0, 5000);
    }

    #[test]
    fn test_test_clock_set_unix_millis() {
        let clock = TestClock::new();
        clock.set_unix_millis(1234567890000);
        assert_eq!(clock.unix_millis(), 1234567890000);
    }

    #[test]
    fn test_clock_trait_object() {
        let clock: Arc<dyn Clock> = Arc::new(SystemClock);
        assert!(clock.unix_millis() > 0);

        let test_clock: Arc<dyn Clock> = Arc::new(TestClock::new());
        assert!(test_clock.unix_millis() > 0);
    }
}
