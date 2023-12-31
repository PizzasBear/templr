use proc_macro2::{Punct, Spacing, TokenStream};
use quote::ToTokens;
use syn::{
    ext::*,
    parse::{Parse, ParseStream},
    spanned::Spanned,
    Ident, LitFloat, LitInt, LitStr, Token,
};

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
    fn peek(&self, input: ParseStream) -> bool {
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
}

impl Parse for NamePart {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lookahead1 = input.lookahead1();
        if lookahead1.peek(Ident::peek_any) {
            Ok(Self::Ident(Ident::parse_any(input)?))
        } else if lookahead1.peek(LitInt) {
            Ok(Self::Int(input.parse()?))
        } else if lookahead1.peek(LitFloat) {
            Ok(Self::Float(input.parse()?))
        } else if lookahead1.peek(Token![.]) {
            let punct: Punct = input.parse()?;
            let mut tk: Token![.] = Default::default();
            tk.span = punct.span();
            Ok(Self::Dot(tk, punct.spacing()))
        } else if lookahead1.peek(Token![-]) {
            let punct: Punct = input.parse()?;
            let mut tk: Token![-] = Default::default();
            tk.span = punct.span();
            Ok(Self::Hyphen(tk, punct.spacing()))
        } else if lookahead1.peek(Token![:]) {
            let punct: Punct = input.parse()?;
            let mut tk: Token![:] = Default::default();
            tk.span = punct.span();
            Ok(Self::Colon(tk, punct.spacing()))
        } else if lookahead1.peek(Token![@]) {
            let punct: Punct = input.parse()?;
            let mut tk: Token![@] = Default::default();
            tk.span = punct.span();
            Ok(Self::At(tk, punct.spacing()))
        } else if lookahead1.peek(Token![?]) {
            let punct: Punct = input.parse()?;
            let mut tk: Token![?] = Default::default();
            tk.span = punct.span();
            Ok(Self::Question(tk, punct.spacing()))
        } else {
            Err(lookahead1.error())
        }
    }
}

#[derive(Debug, Clone)]
pub enum Name {
    Str(LitStr),
    Parts(Vec<NamePart>),
}

impl Name {
    pub fn to_string(&self) -> String {
        match self {
            Self::Str(s) => s.value(),
            Self::Parts(parts) => {
                let mut s = String::new();
                for part in parts {
                    match part {
                        NamePart::Ident(part) => s.push_str(&part.to_string()),
                        NamePart::Int(part) => s.push_str(&part.to_string()),
                        NamePart::Float(part) => s.push_str(&part.to_string()),
                        NamePart::Dot(_, _) => s.push('.'),
                        NamePart::Hyphen(_, _) => s.push('-'),
                        NamePart::Colon(_, _) => s.push(':'),
                        NamePart::At(_, _) => s.push('@'),
                        NamePart::Question(_, _) => s.push('?'),
                    }
                }
                s
            }
        }
    }
}

impl Parse for Name {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(LitStr) {
            Ok(Self::Str(input.parse()?))
        } else {
            let mut parts = vec![NamePart::parse(input)?];
            while parts.last().unwrap().peek(input) {
                parts.push(input.parse()?);
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
                for part in parts {
                    part.to_tokens(tokens);
                }
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
    let name: Name = syn::parse_quote! {
        @on::hello-world.camel
    };
    let name2: Name = syn::parse_quote! {
        "@on::hello-world.camel"
    };
    assert_eq!(name, name2);
}
