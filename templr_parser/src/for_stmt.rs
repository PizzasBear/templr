use proc_macro2::TokenStream;
use quote::{ToTokens, TokenStreamExt};
use syn::{
    braced,
    parse::{Parse, ParseStream},
    token::{self, Brace},
};

use crate::parse_to_vec;

#[derive(Debug, Clone)]
pub struct For<T> {
    pub pound: token::Pound,
    pub for_token: token::For,
    pub pat: Box<syn::Pat>,
    pub in_token: token::In,
    pub expr: Box<syn::Expr>,
    pub brace: Brace,
    pub body: Vec<T>,
}

impl<T: Parse> Parse for For<T> {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        Ok(Self {
            pound: input.parse()?,
            for_token: input.parse()?,
            pat: Box::new(syn::Pat::parse_multi_with_leading_vert(input)?),
            in_token: input.parse()?,
            expr: Box::new(syn::Expr::parse_without_eager_brace(input)?),
            brace: braced!(content in input),
            body: parse_to_vec(&content)?,
        })
    }
}

impl<T: ToTokens> ToTokens for For<T> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.pound.to_tokens(tokens);
        self.for_token.to_tokens(tokens);
        self.pat.to_tokens(tokens);
        self.in_token.to_tokens(tokens);
        self.expr.to_tokens(tokens);
        self.brace.surround(tokens, |tokens| {
            tokens.append_all(&self.body);
        });
    }
}
