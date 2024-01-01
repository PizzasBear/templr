use std::{fmt, io, marker::PhantomData, ops};

pub use anyhow::{anyhow, Error, Result};
pub use templr_macros::templ;

#[doc(hidden)]
struct _DynTemplate(dyn Template);
pub trait Template<Ctx = ()> {
    fn size_hint(&self) -> usize;

    fn render_into(
        &self,
        writer: &mut dyn fmt::Write,
        ctx: &Ctx,
        children: &dyn Template<Ctx>,
    ) -> Result<()>;

    fn render(&self, ctx: &Ctx, children: &dyn Template<Ctx>) -> Result<String> {
        let mut buf = String::new();
        let _ = buf.try_reserve(self.size_hint());
        self.render_into(&mut buf, ctx, children)?;
        Ok(buf)
    }

    fn write_into(
        &self,
        writer: &mut dyn io::Write,
        ctx: &Ctx,
        children: &dyn Template<Ctx>,
    ) -> io::Result<()> {
        // Create a shim which translates an `io::Write` to an `fmt::Write` and saves
        // off I/O errors. instead of discarding them
        struct Adapter<'a, T: ?Sized + 'a> {
            inner: &'a mut T,
            error: io::Result<()>,
        }

        impl<T: io::Write + ?Sized> fmt::Write for Adapter<'_, T> {
            fn write_str(&mut self, s: &str) -> fmt::Result {
                match self.inner.write_all(s.as_bytes()) {
                    Ok(()) => Ok(()),
                    Err(e) => {
                        self.error = Err(e);
                        Err(fmt::Error)
                    }
                }
            }
        }

        struct DisplayError;
        impl fmt::Display for DisplayError {
            fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
                Err(fmt::Error)
            }
        }

        let mut output = Adapter {
            inner: writer,
            error: Ok(()),
        };
        match self.render_into(&mut output, ctx, children) {
            Ok(()) => Ok(()),
            Err(err) => {
                // check if the error came from the underlying `Write` or not
                if output.error.is_err() {
                    output.error
                } else {
                    match err {
                        err => Err(io::Error::new(io::ErrorKind::Other, err)),
                    }
                }
            }
        }
    }
}

impl<Ctx> Template<Ctx> for () {
    fn size_hint(&self) -> usize {
        0
    }
    fn render_into(
        &self,
        _writer: &mut dyn fmt::Write,
        _ctx: &Ctx,
        _children: &dyn Template<Ctx>,
    ) -> Result<()> {
        Ok(())
    }
    fn render(&self, _ctx: &Ctx, _children: &dyn Template<Ctx>) -> Result<String> {
        Ok(String::new())
    }
    fn write_into(
        &self,
        _writer: &mut dyn io::Write,
        _ctx: &Ctx,
        _children: &dyn Template<Ctx>,
    ) -> io::Result<()> {
        Ok(())
    }
}

impl<Ctx, T: Template<Ctx> + ?Sized> Template<Ctx> for &'_ T {
    fn size_hint(&self) -> usize {
        T::size_hint(self)
    }
    fn render_into(
        &self,
        writer: &mut dyn fmt::Write,
        ctx: &Ctx,
        children: &dyn Template<Ctx>,
    ) -> Result<()> {
        T::render_into(self, writer, ctx, children)
    }
    fn render(&self, ctx: &Ctx, children: &dyn Template<Ctx>) -> Result<String> {
        T::render(self, ctx, children)
    }
    fn write_into(
        &self,
        writer: &mut dyn io::Write,
        ctx: &Ctx,
        children: &dyn Template<Ctx>,
    ) -> io::Result<()> {
        T::write_into(self, writer, ctx, children)
    }
}

#[derive(Debug)]
pub struct FnTemplate<Ctx, F>
where
    F: for<'a> Fn(&'a mut dyn fmt::Write, &'a Ctx, &'a dyn Template<Ctx>) -> Result<()>,
{
    size_hint: usize,
    render_into: F,
    _phantom: PhantomData<Ctx>,
}

impl<Ctx, F> FnTemplate<Ctx, F>
where
    F: for<'a> Fn(&'a mut dyn fmt::Write, &'a Ctx, &'a dyn Template<Ctx>) -> Result<()>,
{
    #[inline]
    pub fn new(render_into: F) -> Self {
        Self {
            size_hint: 20,
            render_into,
            _phantom: PhantomData,
        }
    }

    #[inline]
    pub fn new_sized(render_into: F, size_hint: usize) -> Self {
        Self {
            size_hint,
            render_into,
            _phantom: PhantomData,
        }
    }
}

impl<Ctx, F> Template<Ctx> for FnTemplate<Ctx, F>
where
    F: for<'a> Fn(&'a mut dyn fmt::Write, &'a Ctx, &'a dyn Template<Ctx>) -> Result<()>,
{
    fn size_hint(&self) -> usize {
        self.size_hint
    }
    fn render_into(
        &self,
        writer: &mut dyn fmt::Write,
        ctx: &Ctx,
        children: &dyn Template<Ctx>,
    ) -> Result<()> {
        (self.render_into)(writer, ctx, children)
    }
}

/// Write an attribute and check its validity.
fn write_attribute(
    writer: &mut (impl fmt::Write + ?Sized),
    value: &(impl fmt::Display + ?Sized),
) -> Result<()> {
    use fmt::Write;

    pub struct AttributeWriter<'a, W: fmt::Write + ?Sized> {
        has_written: bool,
        writer: &'a mut W,
    }

    impl<W: fmt::Write + ?Sized> fmt::Write for AttributeWriter<'_, W> {
        fn write_str(&mut self, s: &str) -> fmt::Result {
            #[rustfmt::skip]
            fn is_invalid_attribute_char(ch: char) -> bool {
                matches!(
                    ch,
                    '\0'..='\x1F' | '\x7F'..='\u{9F}'
                    | ' ' | '"' | '\'' | '>' | '/' | '='
                    | '\u{FDD0}'..='\u{FDEF}'
                    | '\u{0FFFE}' | '\u{0FFFF}' | '\u{01FFFE}' | '\u{01FFFF}' | '\u{2FFFE}'
                    | '\u{2FFFF}' | '\u{3FFFE}' | '\u{03FFFF}' | '\u{04FFFE}' | '\u{4FFFF}'
                    | '\u{5FFFE}' | '\u{5FFFF}' | '\u{06FFFE}' | '\u{06FFFF}' | '\u{7FFFE}'
                    | '\u{7FFFF}' | '\u{8FFFE}' | '\u{08FFFF}' | '\u{09FFFE}' | '\u{9FFFF}'
                    | '\u{AFFFE}' | '\u{AFFFF}' | '\u{0BFFFE}' | '\u{0BFFFF}' | '\u{CFFFE}'
                    | '\u{CFFFF}' | '\u{DFFFE}' | '\u{0DFFFF}' | '\u{0EFFFE}' | '\u{EFFFF}'
                    | '\u{FFFFE}' | '\u{FFFFF}' | '\u{10FFFE}' | '\u{10FFFF}'
                )
            }

            self.has_written |= !s.is_empty();
            if s.contains(is_invalid_attribute_char) {
                return Err(fmt::Error);
            }
            self.writer.write_str(s)
        }
    }

    let mut attr_writer = AttributeWriter {
        has_written: false,
        writer,
    };

    write!(attr_writer, "{value}")?;

    if !attr_writer.has_written {
        return Err(anyhow!(
            "attribute doesn't conform to html standard: {:?}",
            value.to_string()
        ));
    }

    Ok(())
}

mod sealed {
    pub trait Attribute {}
    pub trait Attributes {}
    pub trait OptAttrValue {}
    pub trait Escapable {}
}

pub trait Escapable: sealed::Escapable {
    const DONT_ESCAPE: bool = false;
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result;
}

impl<T: fmt::Display> sealed::Escapable for T {}
impl<T: fmt::Display> Escapable for T {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

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

impl<T: fmt::Display> sealed::Escapable for Trust<T> {}
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

/// The attribute trait, this will write a single attribute.
pub trait Attribute: sealed::Attribute {
    /// Renders the attribute to the given fmt writer, the attribute will have a space prefixed.
    fn render_into(&self, writer: &mut (impl fmt::Write + ?Sized)) -> Result<()>;
}

impl<T: Attribute + ?Sized> sealed::Attribute for &'_ T {}
impl<T: Attribute + ?Sized> Attribute for &'_ T {
    #[inline]
    fn render_into(&self, writer: &mut (impl fmt::Write + ?Sized)) -> Result<()> {
        T::render_into(self, writer)
    }
}

impl<T: Attribute + ?Sized> sealed::Attribute for &'_ mut T {}
impl<T: Attribute + ?Sized> Attribute for &'_ mut T {
    #[inline]
    fn render_into(&self, writer: &mut (impl fmt::Write + ?Sized)) -> Result<()> {
        T::render_into(self, writer)
    }
}

impl sealed::Attribute for String {}
impl Attribute for String {
    /// Writes a valueless attribute
    fn render_into(&self, writer: &mut (impl fmt::Write + ?Sized)) -> Result<()> {
        writer.write_char(' ')?;
        write_attribute(writer, self)?;

        Ok(())
    }
}

impl sealed::Attribute for str {}
impl Attribute for str {
    /// Writes a valueless attribute
    fn render_into(&self, writer: &mut (impl fmt::Write + ?Sized)) -> Result<()> {
        writer.write_char(' ')?;
        write_attribute(writer, self)?;

        Ok(())
    }
}

impl<N: fmt::Display, T: fmt::Display> sealed::Attribute for (N, T) {}
impl<N: fmt::Display, T: fmt::Display> Attribute for (N, T) {
    fn render_into(&self, writer: &mut (impl fmt::Write + ?Sized)) -> Result<()> {
        writer.write_char(' ')?;
        write_attribute(writer, &self.0)?;
        writer.write_str("=\"")?;
        write_escaped(writer, &self.1)?;
        writer.write_char('"')?;

        Ok(())
    }
}

/// The attributes trait, this can write a variable amount of attributes.
/// You can use this within a template using a braced attribute, `{...}`.
///
/// ```rust
/// let style_attr = ("style", "border: 1px solid black");
/// let html = russx::tmpl! {
///     <div {style_attr}>
///         "hello world"
///     </div>
/// }.render()?;
/// ```
pub trait Attributes: sealed::Attributes {
    /// Renders the attributes to the given fmt writer, the attributes will be separated by spaces
    /// and be prefixed with a space.
    fn render_into(self, writer: &mut (impl fmt::Write + ?Sized)) -> Result<()>;
}

impl<T: Attribute> sealed::Attributes for T {}
impl<T: Attribute> Attributes for T {
    fn render_into(self, writer: &mut (impl fmt::Write + ?Sized)) -> Result<()> {
        Attribute::render_into(&self, writer)
    }
}

impl sealed::Attributes for () {}
impl Attributes for () {
    /// Does nothing
    #[inline]
    fn render_into(self, _writer: &mut (impl fmt::Write + ?Sized)) -> Result<()> {
        Ok(())
    }
}

impl<I: Attribute, T: IntoIterator<Item = I>> sealed::Attributes for ops::RangeTo<T> {}
impl<I: Attribute, T: IntoIterator<Item = I>> Attributes for ops::RangeTo<T> {
    fn render_into(self, writer: &mut (impl fmt::Write + ?Sized)) -> Result<()> {
        for attr in self.end {
            attr.render_into(writer)?;
        }

        Ok(())
    }
}

pub trait OptAttrValue: sealed::OptAttrValue {
    fn to_opt_attr_value(self) -> Option<impl Escapable>;
}

impl sealed::OptAttrValue for bool {}
impl OptAttrValue for bool {
    fn to_opt_attr_value(self) -> Option<impl Escapable> {
        struct Nothing;
        impl sealed::Escapable for Nothing {}
        impl Escapable for Nothing {
            const DONT_ESCAPE: bool = true;
            #[inline]
            fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
                Ok(())
            }
        }
        match self {
            true => Some(Nothing),
            false => None,
        }
    }
}

impl<T: fmt::Display> sealed::OptAttrValue for Option<T> {}
impl<T: fmt::Display> OptAttrValue for Option<T> {
    fn to_opt_attr_value(self) -> Option<impl Escapable> {
        struct Prequal<T: fmt::Display>(T);
        impl<T: fmt::Display> sealed::Escapable for Prequal<T> {}
        impl<T: fmt::Display> Escapable for Prequal<T> {
            const DONT_ESCAPE: bool = true;
            #[inline]
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.write_str("=\"")?;
                write_escaped(f, &self.0)?;
                f.write_str("\"")?;
                Ok(())
            }
        }
        self.map(Prequal)
    }
}
