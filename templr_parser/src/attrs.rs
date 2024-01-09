use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::{
    ext::*,
    parse::{Parse, ParseStream},
    token::Brace,
    Ident, LitFloat, LitInt, LitStr, Token,
};

use crate::{Block, If, Let, Match, Name, Scope};

#[derive(Debug, Clone)]
pub enum HtmlAttrValue {
    Ident(Token![=], Ident),
    Float(Token![=], LitFloat),
    Int(Token![=], LitInt),
    Str(Token![=], LitStr),
    Block(Option<Token![?]>, Token![=], Block),
    None,
}

impl Parse for HtmlAttrValue {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(Token![=]) {
            let eq = input.parse()?;
            let lookahead1 = input.lookahead1();
            if lookahead1.peek(LitStr) {
                Ok(Self::Str(eq, input.parse()?))
            } else if lookahead1.peek(LitInt) {
                Ok(Self::Int(eq, input.parse()?))
            } else if lookahead1.peek(LitFloat) {
                Ok(Self::Float(eq, input.parse()?))
            } else if lookahead1.peek(Ident::peek_any) {
                Ok(Self::Ident(eq, Ident::parse_any(input)?))
            } else if lookahead1.peek(Brace) {
                Ok(Self::Block(None, eq, input.parse()?))
            } else {
                Err(lookahead1.error())
            }
        } else if input.peek(Token![?]) {
            Ok(Self::Block(
                Some(input.parse()?),
                input.parse()?,
                input.parse()?,
            ))
        } else {
            Ok(Self::None)
        }
    }
}

impl ToTokens for HtmlAttrValue {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Ident(eq, slf) => {
                eq.to_tokens(tokens);
                slf.to_tokens(tokens);
            }
            Self::Str(eq, slf) => {
                eq.to_tokens(tokens);
                slf.to_tokens(tokens);
            }
            Self::Int(eq, slf) => {
                eq.to_tokens(tokens);
                slf.to_tokens(tokens);
            }
            Self::Float(eq, slf) => {
                eq.to_tokens(tokens);
                slf.to_tokens(tokens);
            }
            Self::Block(eq, toggle, slf) => {
                eq.to_tokens(tokens);
                toggle.to_tokens(tokens);
                slf.to_tokens(tokens);
            }
            Self::None => {}
        }
    }
}

#[derive(Debug, Clone)]
pub struct HtmlAttr {
    pub name: Name,
    pub value: HtmlAttrValue,
}

impl Parse for HtmlAttr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            name: Name::parse_attr(input)?,
            value: input.parse()?,
        })
    }
}

impl ToTokens for HtmlAttr {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.name.to_tokens(tokens);
        self.value.to_tokens(tokens);
    }
}

#[derive(Debug, Clone)]
pub enum Attr {
    Html(HtmlAttr),
    If(If<Attr>),
    Match(Match<Attr>),
    Scope(Scope<Attr>),
    Let(Let),
    Spread(Block),
}

impl Parse for Attr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(Token![#]) {
            let fork = input.fork();
            let _: Token![#] = fork.parse()?;

            let lookahead1 = fork.lookahead1();
            if lookahead1.peek(Token![if]) {
                Ok(Self::If(input.parse()?))
            } else if lookahead1.peek(Token![match]) {
                Ok(Self::Match(input.parse()?))
            } else if lookahead1.peek(Brace) {
                Ok(Self::Scope(input.parse()?))
            } else if lookahead1.peek(Token![let]) {
                Ok(Self::Let(input.parse()?))
            } else {
                Err(lookahead1.error())
            }
        } else if input.peek(Brace) {
            Ok(Self::Spread(input.parse()?))
        } else {
            Ok(Self::Html(input.parse()?))
        }
    }
}

impl ToTokens for Attr {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Html(slf) => slf.to_tokens(tokens),
            Self::If(slf) => slf.to_tokens(tokens),
            Self::Match(slf) => slf.to_tokens(tokens),
            Self::Scope(slf) => slf.to_tokens(tokens),
            Self::Let(slf) => slf.to_tokens(tokens),
            Self::Spread(slf) => slf.to_tokens(tokens),
        }
    }
}
