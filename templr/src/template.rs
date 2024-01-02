use std::{fmt, io, marker::PhantomData};

use crate::Result;

/// Asserts that `Template` is object safe.
struct _DynTemplate(dyn Template);

pub trait Template<Ctx = ()> {
    fn size_hint(&self) -> usize;

    fn render_with_children_into(
        &self,
        writer: &mut dyn fmt::Write,
        ctx: &Ctx,
        _children: &dyn Template<Ctx>,
    ) -> Result<()>;

    fn render_into(&self, writer: &mut dyn fmt::Write, ctx: &Ctx) -> Result<()> {
        self.render_with_children_into(writer, ctx, &())
    }

    fn render(&self, ctx: &Ctx) -> Result<String> {
        let mut buf = String::new();
        let _ = buf.try_reserve(self.size_hint());
        self.render_into(&mut buf, ctx)?;
        Ok(buf)
    }

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
