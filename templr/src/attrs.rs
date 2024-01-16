use std::{fmt, ops};

use anyhow::anyhow;

use crate::{write_escaped, Escapable, Result};

pub mod class;

pub use class::ClassAttr;

mod sealed {
    pub use crate::sealed::EscapableSeal;
    pub trait AttributeSeal {}
    pub trait AttributesSeal {}
    pub trait OptAttrValueSeal {}
}

/// Write an attribute and check its validity.
fn write_attribute_name(
    writer: &mut (impl fmt::Write + ?Sized),
    name: &(impl fmt::Display + ?Sized),
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

            if s.contains(is_invalid_attribute_char) {
                return Err(fmt::Error);
            }
            self.has_written |= !s.is_empty();
            self.writer.write_str(s)
        }
    }

    let mut attr_writer = AttributeWriter {
        has_written: false,
        writer,
    };

    write!(attr_writer, "{name}")?;
    if !attr_writer.has_written {
        return Err(anyhow!("attribute name cannot be empty"));
    }

    Ok(())
}

/// The attribute trait, this will write a single attribute.
pub trait Attribute: sealed::AttributeSeal {
    /// Renders the attribute to the given fmt writer, the attribute will have a space prefixed.
    fn render_into(&self, writer: &mut (impl fmt::Write + ?Sized)) -> Result<()>;
}

impl<T: Attribute + ?Sized> sealed::AttributeSeal for &'_ T {}
impl<T: Attribute + ?Sized> Attribute for &'_ T {
    #[inline]
    fn render_into(&self, writer: &mut (impl fmt::Write + ?Sized)) -> Result<()> {
        T::render_into(self, writer)
    }
}

impl<T: Attribute + ?Sized> sealed::AttributeSeal for &'_ mut T {}
impl<T: Attribute + ?Sized> Attribute for &'_ mut T {
    #[inline]
    fn render_into(&self, writer: &mut (impl fmt::Write + ?Sized)) -> Result<()> {
        T::render_into(self, writer)
    }
}

impl sealed::AttributeSeal for String {}
impl Attribute for String {
    /// Writes a valueless attribute
    fn render_into(&self, writer: &mut (impl fmt::Write + ?Sized)) -> Result<()> {
        writer.write_char(' ')?;
        write_attribute_name(writer, self)?;

        Ok(())
    }
}

impl sealed::AttributeSeal for str {}
impl Attribute for str {
    /// Writes a valueless attribute
    fn render_into(&self, writer: &mut (impl fmt::Write + ?Sized)) -> Result<()> {
        writer.write_char(' ')?;
        write_attribute_name(writer, self)?;

        Ok(())
    }
}

impl<N: fmt::Display, T: fmt::Display> sealed::AttributeSeal for (N, T) {}
impl<N: fmt::Display, T: fmt::Display> Attribute for (N, T) {
    fn render_into(&self, writer: &mut (impl fmt::Write + ?Sized)) -> Result<()> {
        writer.write_char(' ')?;
        write_attribute_name(writer, &self.0)?;
        writer.write_str("=\"")?;
        write_escaped(writer, &self.1)?;
        writer.write_char('"')?;

        Ok(())
    }
}

impl<N: fmt::Display> sealed::AttributeSeal for (N,) {}
impl<N: fmt::Display> Attribute for (N,) {
    fn render_into(&self, writer: &mut (impl fmt::Write + ?Sized)) -> Result<()> {
        writer.write_char(' ')?;
        write_attribute_name(writer, &self.0)?;

        Ok(())
    }
}

impl<A: Attribute> sealed::AttributeSeal for Option<A> {}
impl<A: Attribute> Attribute for Option<A> {
    fn render_into(&self, writer: &mut (impl fmt::Write + ?Sized)) -> Result<()> {
        match self {
            Some(attr) => attr.render_into(writer),
            None => Ok(()),
        }
    }
}

/// The attributes trait, this can write a variable amount of attributes.
/// You can use this within [`templ!`][crate::templ] using a braced attribute, `{...}`.
///
/// ```rust
/// # use templr::{Template, templ};
/// let style_attr = ("style", "border: 1px solid black");
/// let t = templ! {
///     <div {style_attr}>
///         hello world
///     </div>
/// };
///
/// let html = t.render(&()).unwrap();
/// assert_eq!(html, r#"<div style="border: 1px solid black">hello world</div>"#);
/// ```
pub trait Attributes: sealed::AttributesSeal {
    /// Renders the attributes to the given fmt writer, the attributes will be separated by spaces
    /// and be prefixed with a space.
    fn render_into(self, writer: &mut (impl fmt::Write + ?Sized)) -> Result<()>;
}

impl<T: Attribute> sealed::AttributesSeal for T {}
impl<T: Attribute> Attributes for T {
    fn render_into(self, writer: &mut (impl fmt::Write + ?Sized)) -> Result<()> {
        Attribute::render_into(&self, writer)
    }
}

impl sealed::AttributesSeal for () {}
impl Attributes for () {
    /// Does nothing
    #[inline]
    fn render_into(self, _writer: &mut (impl fmt::Write + ?Sized)) -> Result<()> {
        Ok(())
    }
}

impl<I: Attribute, T: IntoIterator<Item = I>> sealed::AttributesSeal for ops::RangeTo<T> {}
impl<I: Attribute, T: IntoIterator<Item = I>> Attributes for ops::RangeTo<T> {
    fn render_into(self, writer: &mut (impl fmt::Write + ?Sized)) -> Result<()> {
        for attr in self.end {
            attr.render_into(writer)?;
        }

        Ok(())
    }
}

/// The trait for optional attribute values. This is used when `attr?={value}` is evaluated.
pub trait OptAttrValue: sealed::OptAttrValueSeal {
    /// When [`None`] the attribute shouldn't be rendered.
    /// When [`Some(value)`](std::option::Option::Some) the attribute name should be rendered
    /// followed by [`value.fmt(...)`](Escapable::fmt).
    fn to_opt_attr_value(self) -> Option<impl Escapable>;
}

impl sealed::OptAttrValueSeal for bool {}
impl OptAttrValue for bool {
    fn to_opt_attr_value(self) -> Option<impl Escapable> {
        struct Nothing;
        impl sealed::EscapableSeal for Nothing {}
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

impl<T: fmt::Display> sealed::OptAttrValueSeal for Option<T> {}
impl<T: fmt::Display> OptAttrValue for Option<T> {
    fn to_opt_attr_value(self) -> Option<impl Escapable> {
        struct Prequal<T: fmt::Display>(T);
        impl<T: fmt::Display> sealed::EscapableSeal for Prequal<T> {}
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
