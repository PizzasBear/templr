use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::{
    braced,
    parse::{Parse, ParseStream},
    token::{self, Brace},
    Token,
};

use crate::parse_to_vec;

#[derive(Debug, Clone)]
pub struct MatchArm<T> {
    pub pat: Box<syn::Pat>,
    pub guard: Option<(token::If, Box<syn::Expr>)>,
    pub fat_arrow: token::FatArrow,
    pub brace: Brace,
    pub body: Vec<T>,
}

impl<T: Parse> Parse for MatchArm<T> {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        Ok(Self {
            pat: Box::new(syn::Pat::parse_multi_with_leading_vert(input)?),
            guard: match Option::<Token![if]>::parse(input)? {
                Some(tk) => Some((tk, input.parse()?)),
                None => None,
            },
            fat_arrow: input.parse()?,
            brace: braced!(content in input),
            body: parse_to_vec(&content)?,
        })
    }
}

impl<T: ToTokens> ToTokens for MatchArm<T> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.pat.to_tokens(tokens);
        if let Some((if_token, expr)) = &self.guard {
            if_token.to_tokens(tokens);
            expr.to_tokens(tokens);
        }
        self.fat_arrow.to_tokens(tokens);
        self.brace.surround(tokens, |tokens| {
            for item in &self.body {
                item.to_tokens(tokens)
            }
        });
    }
}

#[derive(Debug, Clone)]
pub struct Match<T> {
    pub pound: token::Pound,
    pub match_token: token::Match,
    pub expr: Box<syn::Expr>,
    pub brace: Brace,
    pub arms: Vec<MatchArm<T>>,
}

impl<T: Parse> Parse for Match<T> {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        Ok(Self {
            pound: input.parse()?,
            match_token: input.parse()?,
            expr: Box::new(syn::Expr::parse_without_eager_brace(input)?),
            brace: braced!(content in input),
            arms: parse_to_vec(&content)?,
        })
    }
}

impl<T: ToTokens> ToTokens for Match<T> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.pound.to_tokens(tokens);
        self.match_token.to_tokens(tokens);
        self.expr.to_tokens(tokens);
        self.brace.surround(tokens, |tokens| {
            for arm in &self.arms {
                arm.to_tokens(tokens)
            }
        });
    }
}
