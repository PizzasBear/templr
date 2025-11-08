use core::fmt;
use std::{
    borrow::Cow,
    collections::{hash_set, HashSet},
    error::Error as StdError,
    iter::FusedIterator,
};

use super::{sealed, write_escaped, Attribute};
use crate::Result;

/// A class error, returned when an invalid class is added to `ClassAttr`.
#[derive(Debug, Clone)]
pub enum ClassError {
    /// HTML class with ASCII whitespace
    ClassWithWhitespace,
    /// Empty HTML class
    EmptyClass,
}

impl fmt::Display for ClassError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ClassWithWhitespace => write!(f, "HTML class with ASCII whitespace"),
            Self::EmptyClass => write!(f, "Empty HTML class"),
        }
    }
}

impl StdError for ClassError {}

/// A struct that represents the `class` attribute.
/// This allows you to mutate the set of classes with a specialized API.
///
/// ```rust
/// # use templr::{templ, Template, attrs::ClassAttr};
/// use itertools::Itertools;
///
/// let highlight = true;
/// let t = templ! {
///     <div
///         class={
///             let mut classes = ClassAttr::new("w-2 m-4")?;
///             if highlight {
///                 classes.add_multiple("b-1 b-solid b-black")?;
///             }
///             assert_eq!(
///                 classes.iter().sorted().join(" "),
///                 "b-1 b-black b-solid m-4 w-2",
///             );
///             classes.value()
///         }
///     >
///     </div>
/// };
/// println!("{t}")
/// // Should print something equivalent to:
/// // <div class="w-2 m-4 b-1 b-solid b-black"></div>
/// ```
#[derive(Debug, Clone, Default)]
pub struct ClassAttr {
    classes: HashSet<Cow<'static, str>>,
}

impl ClassAttr {
    /// Creates a new empty `ClassAttr`
    pub fn empty() -> Self {
        ClassAttr {
            classes: HashSet::new(),
        }
    }

    /// Creates a new `ClassAttr` from an ASCII whitespace separated list of classes.
    /// This is equivalent to calling `empty()` and `add_multiple(...)`.
    pub fn new(classes: &'static str) -> Result<Self, ClassError> {
        let mut slf = Self::empty();
        slf.add_multiple(classes)?;
        Ok(slf)
    }

    /// Returns true if the attribute contains no classes.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.classes.is_empty()
    }

    /// Returns the number of classes in the attribute.
    #[inline]
    pub fn len(&self) -> usize {
        self.classes.len()
    }

    /// Returns an unordred space separated list of class names.
    /// This is compatible with the HTML class attribute format.
    ///
    /// ```rust
    /// # use templr::{templ, Template, attrs::ClassAttr};
    /// let t = templ! {
    ///     <div class={ClassAttr::new("beautiful")?.value()}>
    ///         Content
    ///     </div>
    /// };
    /// assert_eq!(
    ///     t.render(&()).unwrap(),
    ///     "<div class=\"beautiful\">Content</div>",
    /// );
    /// ```
    pub fn value(&self) -> impl fmt::Display + Copy + '_ {
        #[derive(Clone, Copy)]
        struct Display<'a>(&'a ClassAttr);

        impl fmt::Display for Display<'_> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                let mut space = false;
                for class in &self.0.classes {
                    if space {
                        write!(f, " ")?;
                    }
                    fmt::Display::fmt(&class, f)?;
                    space = true;
                }
                Ok(())
            }
        }

        Display(self)
    }

    /// This will try to add the `class`.
    /// If the class was successfully added this'll return true.
    /// Otherwise, the class already exists and this'll return false.
    pub fn add(&mut self, class: impl Into<Cow<'static, str>>) -> Result<bool, ClassError> {
        let class = class.into();

        if class.is_empty() {
            return Err(ClassError::EmptyClass);
        } else if class.contains(|ch: char| ch.is_ascii_whitespace()) {
            return Err(ClassError::ClassWithWhitespace);
        }

        Ok(self.classes.insert(class))
    }

    /// Adds multiple classes from the ASCII whitespace separated list `classes`.
    pub fn add_multiple(&mut self, classes: &'static str) -> Result<(), ClassError> {
        for class in classes.split_ascii_whitespace() {
            self.classes.insert(class.into());
        }
        Ok(())
    }

    /// Adds all of the classes from the iterator.
    pub fn add_iter(
        &mut self,
        classes: impl IntoIterator<Item = impl Into<Cow<'static, str>>>,
    ) -> Result<(), ClassError> {
        for class in classes {
            self.add(class)?;
        }
        Ok(())
    }

    /// Removes the class from the class set.
    /// Returns whether the class was present.
    pub fn remove(&mut self, class: &str) -> bool {
        self.classes.remove(class)
    }

    /// Removes each class in the ASCII whitespace separated list `classes`.
    pub fn remove_multiple(&mut self, classes: &str) {
        for class in classes.split_ascii_whitespace() {
            self.remove(class);
        }
    }

    /// Removes each class in the `classes` iterator.
    pub fn remove_iter(&mut self, classes: impl IntoIterator<Item = impl AsRef<str>>) {
        for class in classes {
            self.remove(class.as_ref());
        }
    }

    /// Iterates over all present classes in no particular order.
    pub fn iter(&self) -> Iter<'_> {
        Iter(self.classes.iter())
    }
}

impl IntoIterator for ClassAttr {
    type Item = Cow<'static, str>;
    type IntoIter = hash_set::IntoIter<Self::Item>;
    fn into_iter(self) -> Self::IntoIter {
        self.classes.into_iter()
    }
}

impl<'a> IntoIterator for &'a ClassAttr {
    type Item = &'a str;
    type IntoIter = Iter<'a>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

/// Iterates over all present classes in no particular order.
pub struct Iter<'a>(hash_set::Iter<'a, Cow<'static, str>>);

impl<'a> Iterator for Iter<'a> {
    type Item = &'a str;
    fn next(&mut self) -> Option<&'a str> {
        Some(&**self.0.next()?)
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}
impl ExactSizeIterator for Iter<'_> {
    fn len(&self) -> usize {
        self.0.len()
    }
}
impl FusedIterator for Iter<'_> {}

impl sealed::AttributeSeal for ClassAttr {}

/// This will render to the full class attribute.
///
/// ```rust
/// # use templr::{templ, Template, attrs::ClassAttr};
/// let t = templ! {
///     <div {ClassAttr::new("beautiful")?}>
///         <br {ClassAttr::empty()} />
///     </div>
/// };
/// assert_eq!(
///     t.render(&()).unwrap(),
///     r#"<div class="beautiful"><br class=""></div>"#,
/// );
/// ```
impl Attribute for ClassAttr {
    fn render_into(&self, writer: &mut (impl std::fmt::Write + ?Sized)) -> Result<()> {
        writer.write_str(" class=\"")?;
        write_escaped(writer, &self.value())?;
        writer.write_char('"')?;
        Ok(())
    }
}

#[test]
fn test_class_attr() -> Result<(), ClassError> {
    use itertools::Itertools;

    let mut attr = ClassAttr::new("hello world this-is w-[1px] m-4")?;
    attr.add_iter(["josh", "states", "line", "united"])?;
    attr.remove_multiple("world josh");
    attr.remove_iter(["this-is", "line"]);

    assert_eq!(
        attr.iter().sorted().join(" "),
        "hello m-4 states united w-[1px]"
    );

    Ok(())
}
