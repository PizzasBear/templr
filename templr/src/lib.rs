use std::{fmt, ops};

pub use anyhow::{self, Error, Result};
pub use templr_macros::templ;

mod attrs;
mod template;

pub use {attrs::*, template::*};

mod sealed {
    pub trait EscapableSeal {}
}

/// Mostly like `std::fmt::Display`, but with a `DONT_ESCAPE` flag.
pub trait Escapable: sealed::EscapableSeal {
    /// `true` if you don't want to process the value through the html escaper.
    const DONT_ESCAPE: bool = false;
    /// Formats the value using the given formatter.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result;
}

impl<T: fmt::Display> sealed::EscapableSeal for T {}
impl<T: fmt::Display> Escapable for T {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

/// Trust content and don't escape it when interpolated in templates.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Trust<T>(pub T);

impl<T> ops::Deref for Trust<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.0
    }
}
impl<T> ops::DerefMut for Trust<T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.0
    }
}

impl<T: fmt::Display> sealed::EscapableSeal for Trust<T> {}
impl<T: fmt::Display> Escapable for Trust<T> {
    const DONT_ESCAPE: bool = true;
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

/// Writes html-escaped `value` into `writer`.
pub fn write_escaped<T: Escapable + ?Sized>(
    writer: &mut (impl fmt::Write + ?Sized),
    value: &T,
) -> fmt::Result {
    use fmt::Write;
    pub struct EscapeWriter<'a, W: Write + ?Sized>(&'a mut W);

    impl<W: Write + ?Sized> Write for EscapeWriter<'_, W> {
        #[inline]
        fn write_str(&mut self, s: &str) -> fmt::Result {
            use askama_escape::Escaper;

            askama_escape::Html.write_escaped(&mut *self.0, s)
        }
    }

    struct Display<'a, T: ?Sized>(&'a T);
    impl<T: Escapable + ?Sized> fmt::Display for Display<'_, T> {
        #[inline]
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            self.0.fmt(f)
        }
    }

    if T::DONT_ESCAPE {
        write!(writer, "{}", Display(value))
    } else {
        write!(EscapeWriter(writer), "{}", Display(value))
    }
}
