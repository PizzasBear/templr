use std::{fmt, io, marker::PhantomData};

use crate::Result;

/// Asserts that `Template` is object safe.
struct _DynTemplate(dyn Template);

/// A trait to convert a type to a template. Used when instantiating templates.
///
/// Useful to implement on `struct`s.
///
/// ```rust
/// # use templr::{templ, ToTemplate, Template};
/// struct Greet<'a> {
///     name: &'a str,
/// }
///
/// impl ToTemplate for Greet<'_> {
///     fn to_template(&self) -> impl Template + '_ {
///         templ! {
///             Hello, {self.name}!
///         }
///     }
/// }
///
/// let t = templ! {
///     #(Greet { name: "baba" });
/// };
/// let html = t.render(&()).unwrap();
/// assert_eq!(html, "Hello, baba!");
/// ```
pub trait ToTemplate<Ctx = ()> {
    /// Converts this type into the template.
    fn to_template(&self) -> impl Template<Ctx> + '_;
}

impl<Ctx, T: Template<Ctx>> ToTemplate<Ctx> for T {
    fn to_template(&self) -> impl Template<Ctx> + '_ {
        self
    }
}

/// Main template trait.
/// An implementation can be generated using both the `templ!` macro.
pub trait Template<Ctx = ()> {
    /// Provides a rough estimate of the expanded length of the rendered template.
    /// Larger values result in higher memory usage but fewer reallocations.
    /// Smaller values result in the opposite. This value only affects render.
    /// It does not take effect when calling `render_into`, `write_into`, the `fmt::Display`
    // implementation, or the blanket `ToString::to_string` implementation.
    fn size_hint(&self) -> usize;

    /// Renders the template to the given fmt writer.
    fn render_with_children_into(
        &self,
        writer: &mut dyn fmt::Write,
        ctx: &Ctx,
        children: &dyn Template<Ctx>,
    ) -> Result<()>;

    /// Renders the template to the given fmt writer,
    /// assuming that there are not children.
    fn render_into(&self, writer: &mut dyn fmt::Write, ctx: &Ctx) -> Result<()> {
        self.render_with_children_into(writer, ctx, &())
    }

    fn render(&self, ctx: &Ctx) -> Result<String> {
        let mut buf = String::new();
        let _ = buf.try_reserve(self.size_hint());
        self.render_into(&mut buf, ctx)?;
        Ok(buf)
    }

    /// Renders the template to the given IO writer.
    fn write_into(&self, writer: &mut dyn io::Write, ctx: &Ctx) -> io::Result<()> {
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
        match self.render_into(&mut output, ctx) {
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
    fn render_with_children_into(
        &self,
        _writer: &mut dyn fmt::Write,
        _ctx: &Ctx,
        _children: &dyn Template<Ctx>,
    ) -> Result<()> {
        Ok(())
    }
    fn render_into(&self, _writer: &mut dyn fmt::Write, _ctx: &Ctx) -> Result<()> {
        Ok(())
    }
    fn render(&self, _ctx: &Ctx) -> Result<String> {
        Ok(String::new())
    }
    fn write_into(&self, _writer: &mut dyn io::Write, _ctx: &Ctx) -> io::Result<()> {
        Ok(())
    }
}

impl<Ctx, T: Template<Ctx> + ?Sized> Template<Ctx> for &'_ T {
    fn size_hint(&self) -> usize {
        T::size_hint(self)
    }
    fn render_with_children_into(
        &self,
        writer: &mut dyn fmt::Write,
        ctx: &Ctx,
        children: &dyn Template<Ctx>,
    ) -> Result<()> {
        T::render_with_children_into(self, writer, ctx, children)
    }
    fn render_into(&self, writer: &mut dyn fmt::Write, ctx: &Ctx) -> Result<()> {
        T::render_into(self, writer, ctx)
    }
    fn render(&self, ctx: &Ctx) -> Result<String> {
        T::render(self, ctx)
    }
    fn write_into(&self, writer: &mut dyn io::Write, ctx: &Ctx) -> io::Result<()> {
        T::write_into(self, writer, ctx)
    }
}

/// A wrapper for the `render_into` function that implements `Template`.
#[derive(Debug)]
pub struct FnTemplate<F, Ctx = ()>
where
    F: for<'a> Fn(&'a mut dyn fmt::Write, &'a Ctx, &'a dyn Template<Ctx>) -> Result<()>,
{
    size_hint: usize,
    render_into: F,
    _phantom: PhantomData<Ctx>,
}

impl<F, Ctx> FnTemplate<F, Ctx>
where
    F: for<'a> Fn(&'a mut dyn fmt::Write, &'a Ctx, &'a dyn Template<Ctx>) -> Result<()>,
{
    /// Creates a new `TemplateFn` a render_into function (see `Template::render_into`).
    /// This template will have the default size.
    #[inline]
    pub fn new(render_into: F) -> Self {
        Self {
            size_hint: 80,
            render_into,
            _phantom: PhantomData,
        }
    }

    /// Creates a new `TemplateFn` from a size hint (see `Template::SIZE_HINT`) and a render_into
    /// function (see `Template::render_into`).
    #[inline]
    pub fn new_sized(size_hint: usize, render_into: F) -> Self {
        Self {
            size_hint,
            render_into,
            _phantom: PhantomData,
        }
    }
}

impl<F, Ctx> Template<Ctx> for FnTemplate<F, Ctx>
where
    F: for<'a> Fn(&'a mut dyn fmt::Write, &'a Ctx, &'a dyn Template<Ctx>) -> Result<()>,
{
    fn size_hint(&self) -> usize {
        self.size_hint
    }
    fn render_with_children_into(
        &self,
        writer: &mut dyn fmt::Write,
        ctx: &Ctx,
        children: &dyn Template<Ctx>,
    ) -> Result<()> {
        (self.render_into)(writer, ctx, children)
    }
}
