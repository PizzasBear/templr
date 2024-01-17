use std::{fmt, io, marker::PhantomData};

pub use templr_macros::Template;

use crate::Result;

/// Asserts that [`Template`] is object safe.
struct _DynTemplate(dyn Template);

/// A trait to convert a type to a [`Template`]. Used when deriving [`Template`].
///
/// Useful to implement on `struct`s.
///
/// ```rust
/// # use templr::{templ, ToTemplate, Template};
/// #[derive(Template)]
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
pub trait ToTemplate<Ctx: ?Sized = ()> {
    /// Converts this type into the template.
    /// This function should be as lightweight as possible.
    fn to_template(&self) -> impl Template<Ctx> + '_;
}

impl<Ctx: ?Sized, T: ToTemplate<Ctx> + ?Sized> ToTemplate<Ctx> for &'_ T {
    fn to_template(&self) -> impl Template<Ctx> + '_ {
        T::to_template(self)
    }
}

// impl<Ctx: ?Sized, T: ToTemplate<Ctx>> Template<Ctx> for T {
//     fn size_hint(&self) -> usize {
//         self.to_template().size_hint()
//     }
//     fn render_with_children_into(
//         &self,
//         writer: &mut dyn fmt::Write,
//         ctx: &Ctx,
//         children: &dyn Template<Ctx>,
//     ) -> Result<()> {
//         self.to_template()
//             .render_with_children_into(writer, ctx, children)
//     }
//     fn render_into(&self, writer: &mut dyn fmt::Write, ctx: &Ctx) -> Result<()> {
//         self.to_template().render_into(writer, ctx)
//     }
//     fn write_into(&self, writer: &mut dyn io::Write, ctx: &Ctx) -> io::Result<()> {
//         self.to_template().write_into(writer, ctx)
//     }
//     fn render(&self, ctx: &Ctx) -> Result<String> {
//         self.to_template().render(ctx)
//     }
// }

// impl<Ctx: ?Sized, T: Template<Ctx> + ?Sized> ToTemplate<Ctx> for T {
//     fn to_template(&self) -> impl Template<Ctx> + '_ {
//         self
//     }
// }

/// Main template trait.
/// An implementation can be generated using the [`templ!`](crate::templ) macro.
pub trait Template<Ctx: ?Sized = ()> {
    /// Provides a rough estimate of the expanded length of the rendered template.
    /// Larger values result in higher memory usage but fewer reallocations.
    /// Smaller values result in the opposite. This value only affects render.
    /// It does not take effect when calling [`render_into`](Template::render_into),
    /// [`write_into`](Template::write_into), and the [`fmt::Display`] (when implemented).
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
                    Err(io::Error::new(io::ErrorKind::Other, err))
                }
            }
        }
    }
}

impl<Ctx: ?Sized> Template<Ctx> for () {
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

impl<Ctx: ?Sized, T: Template<Ctx> + ?Sized> Template<Ctx> for &'_ T {
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

/// A wrapper for the [`render_with_children_into`](Template::render_with_children_into)
/// closure that implements [`Template`].
#[derive(Debug)]
pub struct FnTemplate<F, Ctx: ?Sized = ()>
where
    F: Fn(&mut dyn fmt::Write, &Ctx, &dyn Template<Ctx>) -> Result<()>,
{
    size_hint: usize,
    render_with_children_into: F,
    _phantom: PhantomData<Ctx>,
}

impl<F, Ctx: ?Sized> FnTemplate<F, Ctx>
where
    F: Fn(&mut dyn fmt::Write, &Ctx, &dyn Template<Ctx>) -> Result<()>,
{
    /// Creates a new [`FnTemplate`] from
    /// a [`render_with_children_into`](Template::render_with_children_into) closure.
    /// This template will have the default size.
    #[inline]
    pub fn new(render_with_children_into: F) -> Self {
        Self {
            size_hint: 80,
            render_with_children_into,
            _phantom: PhantomData,
        }
    }

    /// Creates a new [`FnTemplate`] from a size hint (see [`Template::size_hint`]) and
    /// a [`render_with_children_into`](Template::render_with_children_into) closure
    #[inline]
    pub fn new_sized(size_hint: usize, render_with_children_into: F) -> Self {
        Self {
            size_hint,
            render_with_children_into,
            _phantom: PhantomData,
        }
    }
}

impl<F, Ctx: ?Sized> Template<Ctx> for FnTemplate<F, Ctx>
where
    F: Fn(&mut dyn fmt::Write, &Ctx, &dyn Template<Ctx>) -> Result<()>,
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
        (self.render_with_children_into)(writer, ctx, children)
    }
}

impl<F> fmt::Display for FnTemplate<F>
where
    F: Fn(&mut dyn fmt::Write, &(), &dyn Template<()>) -> crate::Result<()> + Send,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.render_into(f, &()).map_err(|_| fmt::Error)
    }
}

/// Return type for functions that return [`templ! { ... }`](crate::templ).
/// This takes a lifetime (defaults to `'static`) and a context type (defaults to `()`).
///
/// ```rust
/// # use templr::{templ, templ_ret};
/// fn hello(name: &str) -> templ_ret!['_, ()] {
///     templ! {
///         Hello, {name}!
///     }
/// }
/// ```
/// Instead of:
/// ```rust
/// # use std::fmt;
/// # use templr::{templ, templ_ret, FnTemplate, Template, Result};
/// fn hello(
///     name: &str,
/// ) -> FnTemplate<impl '_ + Fn(&mut dyn fmt::Write, &(), &dyn Template) -> Result<()>> {
///     templ! {
///         Hello, {name}!
///     }
/// }
/// ```
#[macro_export]
macro_rules! templ_ret {
    ($lt:lifetime, $ctx:ty $(,)?) => {
        $crate::FnTemplate<
            impl $lt + Fn(
                &mut dyn ::std::fmt::Write,
                &$ctx,
                &dyn $crate::Template<$ctx>,
            ) -> $crate::Result<()>,
            $ctx,
        >
    };
    ($lt:lifetime $(,)?) => { $crate::templ_ret![$lt, ()] };
    ($ctx:ty $(,)?) => { $crate::templ_ret!['static, $ctx] };
    () => { $crate::templ_ret!['static, ()] };
}
