use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::{
    ext::*,
    parse::{Parse, ParseStream},
    token::Brace,
    Ident, LitStr, Token,
};

use crate::{Block, If, Let, Match, Name, PoundBlock};

#[derive(Debug, Clone)]
pub enum HtmlAttrValue {
    Ident(Ident),
    Str(LitStr),
    Block(Block),
}

impl Parse for HtmlAttrValue {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lookahead1 = input.lookahead1();
        if lookahead1.peek(LitStr) {
            Ok(Self::Str(input.parse()?))
        } else if lookahead1.peek(Ident::peek_any) {
            Ok(Self::Ident(Ident::parse_any(input)?))
        } else if lookahead1.peek(Brace) {
            Ok(Self::Block(input.parse()?))
        } else {
            Err(lookahead1.error())
        }
    }
}

impl ToTokens for HtmlAttrValue {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Ident(slf) => slf.to_tokens(tokens),
            Self::Str(slf) => slf.to_tokens(tokens),
            Self::Block(slf) => slf.to_tokens(tokens),
        }
    }
}

#[derive(Debug, Clone)]
pub struct HtmlAttr {
    pub name: Name,
    pub toggle_flag: Option<Token![?]>,
    pub eq: Token![=],
    pub value: HtmlAttrValue,
}

impl Parse for HtmlAttr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let toggle_flag;
        Ok(Self {
            name: Name::parse_attr(input)?,
            toggle_flag: {
                toggle_flag = input.parse()?;
                toggle_flag
            },
            eq: input.parse()?,
            value: match toggle_flag {
                Some(_) => HtmlAttrValue::Block(input.parse()?),
                None => input.parse()?,
            },
        })
    }
}

impl ToTokens for HtmlAttr {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.name.to_tokens(tokens);
        self.toggle_flag.to_tokens(tokens);
        self.eq.to_tokens(tokens);
        self.value.to_tokens(tokens);
    }
}

#[derive(Debug, Clone)]
pub enum Attr {
    Html(HtmlAttr),
    If(If<Attr>),
    Match(Match<Attr>),
    Block(PoundBlock<Attr>),
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
                Ok(Self::Block(input.parse()?))
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
            Self::Block(slf) => slf.to_tokens(tokens),
            Self::Let(slf) => slf.to_tokens(tokens),
            Self::Spread(slf) => slf.to_tokens(tokens),
        }
    }
}
