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
pub struct ElseIfBranch<T> {
    pub else_token: token::Else,
    pub if_token: token::If,
    pub cond: Box<syn::Expr>,
    pub brace: Brace,
    pub body: Vec<T>,
}

impl<T: Parse> Parse for ElseIfBranch<T> {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        Ok(Self {
            else_token: input.parse()?,
            if_token: input.parse()?,
            cond: Box::new(syn::Expr::parse_without_eager_brace(input)?),
            brace: braced!(content in input),
            body: parse_to_vec(&content)?,
        })
    }
}

impl<T: ToTokens> ToTokens for ElseIfBranch<T> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.else_token.to_tokens(tokens);
        self.if_token.to_tokens(tokens);
        self.cond.to_tokens(tokens);
        self.brace.surround(tokens, |tokens| {
            for item in &self.body {
                item.to_tokens(tokens)
            }
        });
    }
}

#[derive(Debug, Clone)]
pub struct ElseBranch<T> {
    pub else_token: token::Else,
    pub brace: Brace,
    pub body: Vec<T>,
}

impl<T: Parse> Parse for ElseBranch<T> {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        Ok(Self {
            else_token: input.parse()?,
            brace: braced!(content in input),
            body: parse_to_vec(&content)?,
        })
    }
}

impl<T: ToTokens> ToTokens for ElseBranch<T> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.else_token.to_tokens(tokens);
        self.brace.surround(tokens, |tokens| {
            for item in &self.body {
                item.to_tokens(tokens)
            }
        });
    }
}

#[derive(Debug, Clone)]
pub struct If<T> {
    pub pound: token::Pound,
    pub if_token: token::If,
    pub cond: Box<syn::Expr>,
    pub brace: Brace,
    pub body: Vec<T>,
    pub else_if_branches: Vec<ElseIfBranch<T>>,
    pub else_branch: Option<ElseBranch<T>>,
}

impl<T: Parse> Parse for If<T> {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        Ok(Self {
            pound: input.parse()?,
            if_token: input.parse()?,
            cond: Box::new(syn::Expr::parse_without_eager_brace(input)?),
            brace: braced!(content in input),
            body: parse_to_vec(&content)?,
            else_if_branches: {
                let mut branches = vec![];
                while input.peek(Token![else]) && input.peek2(Token![if]) {
                    branches.push(input.parse()?);
                }
                branches
            },
            else_branch: match input.peek(Token![else]) {
                true => Some(input.parse()?),
                false => None,
            },
        })
    }
}

impl<T: ToTokens> ToTokens for If<T> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.pound.to_tokens(tokens);
        self.if_token.to_tokens(tokens);
        self.cond.to_tokens(tokens);
        self.brace.surround(tokens, |tokens| {
            for item in &self.body {
                item.to_tokens(tokens)
            }
        });
        for branch in &self.else_if_branches {
            branch.to_tokens(tokens);
        }
        self.else_branch.to_tokens(tokens);
    }
}
