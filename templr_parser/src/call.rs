use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::{
    braced,
    parse::{Parse, ParseStream},
    token::Brace,
    Token,
};

use crate::TemplBody;

#[derive(Debug, Clone)]
pub enum End {
    Semi(Token![;]),
    Children(Brace, TemplBody),
}

impl ToTokens for End {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Semi(slf) => slf.to_tokens(tokens),
            Self::Children(brace, body) => brace.surround(tokens, |tokens| body.to_tokens(tokens)),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Call {
    pub pound: Token![#],
    pub expr: Box<syn::Expr>,
    pub end: End,
}

impl Parse for Call {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            pound: input.parse()?,
            expr: Box::new(syn::Expr::parse_without_eager_brace(input)?),
            end: {
                let lookahead1 = input.lookahead1();
                if lookahead1.peek(Token![;]) {
                    End::Semi(input.parse()?)
                } else if lookahead1.peek(Brace) {
                    let content;
                    End::Children(braced!(content in input), content.parse()?)
                } else {
                    return Err(lookahead1.error());
                }
            },
        })
    }
}

impl ToTokens for Call {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.pound.to_tokens(tokens);
        self.expr.to_tokens(tokens);
        self.end.to_tokens(tokens);
    }
}
