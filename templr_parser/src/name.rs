use core::fmt;

use proc_macro2::{Punct, Spacing, TokenStream};
use quote::{ToTokens, TokenStreamExt};
use syn::{ext::*, parse::ParseStream, spanned::Spanned, Ident, LitFloat, LitInt, LitStr, Token};

#[rustfmt::skip]
const fn is_invalid_attribute_char(ch: char) -> bool {
    matches!(
        ch,
        '\0'..='\x1F' | '\x7F'..='\u{9F}'
        | 'A'..='Z' | ' ' | '"' | '\'' | '>' | '/' | '='
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

#[rustfmt::skip]
const fn is_invalid_tagname_char(ch: char) -> bool {
    !matches!(
        ch,
        '-' | '.' | '0'..='9' | '_' | 'a'..='z'
        | '\u{B7}' | '\u{C0}'..='\u{D6}' | '\u{D8}'..='\u{F6}' | '\u{F8}'..='\u{37D}'
        | '\u{37F}'..='\u{1FFF}' | '\u{200C}'..='\u{200D}' | '\u{203F}'..='\u{2040}'
        | '\u{2070}'..='\u{218F}' | '\u{2C00}'..='\u{2FEF}' | '\u{3001}'..='\u{D7FF}'
        | '\u{F900}'..='\u{FDCF}' | '\u{FDF0}'..='\u{FFFD}' | '\u{10000}'..='\u{EFFFF}'
    )
}

const INVALID_TAG_MSG: &str =
    "Invalid tag name (https://html.spec.whatwg.org/multipage/syntax.html#syntax-tag-name)";
const INVALID_ATTR_MSG: &str = "Invalid attribute name \
     (https://html.spec.whatwg.org/multipage/syntax.html#syntax-attribute-name)";

#[derive(Debug, Clone)]
pub enum NamePart {
    Ident(Ident),
    Int(LitInt),
    Float(LitFloat),
    Dot(Token![.], Spacing),
    Hyphen(Token![-], Spacing),
    Colon(Token![:], Spacing),
    At(Token![@], Spacing),
    Question(Token![?], Spacing),
}

impl ToTokens for NamePart {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Ident(slf) => slf.to_tokens(tokens),
            Self::Int(slf) => slf.to_tokens(tokens),
            Self::Float(slf) => slf.to_tokens(tokens),
            Self::Dot(slf, _) => slf.to_tokens(tokens),
            Self::Hyphen(slf, _) => slf.to_tokens(tokens),
            Self::Colon(slf, _) => slf.to_tokens(tokens),
            Self::At(slf, _) => slf.to_tokens(tokens),
            Self::Question(slf, _) => slf.to_tokens(tokens),
        }
    }
}

impl NamePart {
    fn peek_attr(&self, input: ParseStream) -> bool {
        let Some((token, _)) = input.cursor().token_tree() else {
            return false;
        };

        if self
            .span()
            .join(token.span())
            .and_then(|span| span.source_text())
            .is_some_and(|source| source.contains(char::is_whitespace))
        {
            return false;
        }

        !matches!(self, Self::Ident(_) | Self::Int(_) | Self::Float(_))
            && (input.peek(Ident::peek_any) || input.peek(LitInt) || input.peek(LitFloat))
            || !matches!(
                self,
                Self::Dot(_, Spacing::Alone)
                    | Self::Hyphen(_, Spacing::Alone)
                    | Self::Colon(_, Spacing::Alone)
                    | Self::At(_, Spacing::Alone)
                    | Self::Question(_, Spacing::Alone)
            ) && (input.peek(Token![.])
                || input.peek(Token![-])
                || input.peek(Token![:])
                || input.peek(Token![@])
                || input.peek(Token![?]) && !input.peek2(Token![=]))
    }

    fn peek_tag(&self, input: ParseStream) -> bool {
        let Some((token, _)) = input.cursor().token_tree() else {
            return false;
        };

        if self
            .span()
            .join(token.span())
            .and_then(|span| span.source_text())
            .is_some_and(|source| source.contains(char::is_whitespace))
        {
            return false;
        }

        const _: () = assert!(is_invalid_tagname_char('@'));
        const _: () = assert!(is_invalid_tagname_char(':'));
        const _: () = assert!(is_invalid_tagname_char('?'));

        !matches!(self, Self::Ident(_) | Self::Int(_) | Self::Float(_))
            && (input.peek(Ident::peek_any) || input.peek(LitInt) || input.peek(LitFloat))
            || !matches!(
                self,
                Self::Dot(_, Spacing::Alone)
                    | Self::Hyphen(_, Spacing::Alone)
                    | Self::Colon(_, Spacing::Alone)
                    | Self::At(_, Spacing::Alone)
                    | Self::Question(_, Spacing::Alone)
            ) && (input.peek(Token![.]) || input.peek(Token![-]))
    }

    pub fn parse_attr(input: ParseStream) -> syn::Result<Self> {
        let lookahead1 = input.lookahead1();

        let slf = if lookahead1.peek(Ident::peek_any) {
            Self::Ident(Ident::parse_any(input)?)
        } else if lookahead1.peek(LitInt) {
            Self::Int(input.parse()?)
        } else if lookahead1.peek(LitFloat) {
            Self::Float(input.parse()?)
        } else if lookahead1.peek(Token![.]) {
            let punct: Punct = input.parse()?;
            let mut tk: Token![.] = Default::default();
            tk.spans[0] = punct.span();
            Self::Dot(tk, punct.spacing())
        } else if lookahead1.peek(Token![-]) {
            let punct: Punct = input.parse()?;
            let mut tk: Token![-] = Default::default();
            tk.spans[0] = punct.span();
            Self::Hyphen(tk, punct.spacing())
        } else if lookahead1.peek(Token![:]) {
            let punct: Punct = input.parse()?;
            let mut tk: Token![:] = Default::default();
            tk.spans[0] = punct.span();
            Self::Colon(tk, punct.spacing())
        } else if lookahead1.peek(Token![@]) {
            let punct: Punct = input.parse()?;
            let mut tk: Token![@] = Default::default();
            tk.spans[0] = punct.span();
            Self::At(tk, punct.spacing())
        } else if lookahead1.peek(Token![?]) {
            let punct: Punct = input.parse()?;
            let mut tk: Token![?] = Default::default();
            tk.spans[0] = punct.span();
            Self::Question(tk, punct.spacing())
        } else {
            return Err(lookahead1.error());
        };

        if slf.to_string().contains(is_invalid_attribute_char) {
            Err(syn::Error::new_spanned(slf, INVALID_ATTR_MSG))
        } else {
            Ok(slf)
        }
    }

    pub fn parse_tag_first(input: ParseStream) -> syn::Result<Self> {
        let ident = Ident::parse_any(input)?;

        let s = ident.to_string();
        if s.starts_with(|ch: char| !ch.is_ascii_lowercase()) || s.contains(is_invalid_tagname_char)
        {
            Err(syn::Error::new(ident.span(), INVALID_TAG_MSG))
        } else {
            Ok(Self::Ident(ident))
        }
    }

    pub fn parse_tag(input: ParseStream) -> syn::Result<Self> {
        let lookahead1 = input.lookahead1();

        let slf = if lookahead1.peek(Ident::peek_any) {
            Self::Ident(Ident::parse_any(input)?)
        } else if lookahead1.peek(LitInt) {
            Self::Int(input.parse()?)
        } else if lookahead1.peek(LitFloat) {
            Self::Float(input.parse()?)
        } else if lookahead1.peek(Token![.]) {
            let punct: Punct = input.parse()?;
            let mut tk: Token![.] = Default::default();
            tk.spans[0] = punct.span();
            Self::Dot(tk, punct.spacing())
        } else if lookahead1.peek(Token![-]) {
            let punct: Punct = input.parse()?;
            let mut tk: Token![-] = Default::default();
            tk.spans[0] = punct.span();
            Self::Hyphen(tk, punct.spacing())
        } else {
            return Err(lookahead1.error());
        };

        if slf.to_string().contains(is_invalid_tagname_char) {
            Err(syn::Error::new_spanned(slf, INVALID_TAG_MSG))
        } else {
            Ok(slf)
        }
    }
}

impl fmt::Display for NamePart {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NamePart::Ident(part) => fmt::Display::fmt(part, f),
            NamePart::Int(part) => fmt::Display::fmt(part, f),
            NamePart::Float(part) => fmt::Display::fmt(part, f),
            NamePart::Dot(_, _) => f.write_str("."),
            NamePart::Hyphen(_, _) => f.write_str("-"),
            NamePart::Colon(_, _) => f.write_str(":"),
            NamePart::At(_, _) => f.write_str("@"),
            NamePart::Question(_, _) => f.write_str("?"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Name {
    Str(LitStr),
    Parts(Vec<NamePart>),
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Str(s) => fmt::Display::fmt(&s.value(), f),
            Self::Parts(parts) => {
                for part in parts {
                    fmt::Display::fmt(part, f)?;
                }
                Ok(())
            }
        }
    }
}

impl Name {
    pub fn parse_tag(input: ParseStream) -> syn::Result<Self> {
        if input.peek(LitStr) {
            let lit: LitStr = input.parse()?;
            let value = lit.value();
            if value.starts_with(|ch: char| !ch.is_ascii_lowercase())
                || value.contains(is_invalid_tagname_char)
            {
                return Err(syn::Error::new(lit.span(), INVALID_TAG_MSG));
            }
            Ok(Self::Str(lit))
        } else {
            let mut parts = vec![NamePart::parse_tag_first(input)?];
            while parts.last().unwrap().peek_tag(input) {
                parts.push(NamePart::parse_tag(input)?);
            }
            Ok(Self::Parts(parts))
        }
    }
    pub fn parse_attr(input: ParseStream) -> syn::Result<Self> {
        if input.peek(LitStr) {
            let lit: LitStr = input.parse()?;
            let value = lit.value();
            if value.contains(is_invalid_attribute_char) {
                return Err(syn::Error::new(lit.span(), INVALID_ATTR_MSG));
            }
            Ok(Self::Str(lit))
        } else {
            let mut parts = vec![NamePart::parse_attr(input)?];
            while parts.last().unwrap().peek_attr(input) {
                parts.push(NamePart::parse_attr(input)?);
            }
            Ok(Self::Parts(parts))
        }
    }
}

impl ToTokens for Name {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Str(slf) => slf.to_tokens(tokens),
            Self::Parts(parts) => {
                tokens.append_all(parts);
            }
        }
    }
}

impl PartialEq for Name {
    fn eq(&self, other: &Self) -> bool {
        self.to_string() == other.to_string()
    }
}
impl Eq for Name {}

#[test]
fn test_name() {
    use quote::quote;
    use syn::parse::Parser;

    let name = Name::parse_attr
        .parse2(quote! {
            @on::hello-world.camel
        })
        .unwrap();
    let name2 = Name::parse_attr
        .parse2(quote! {
            "@on::hello-world.camel"
        })
        .unwrap();
    assert_eq!(name, name2);
}
