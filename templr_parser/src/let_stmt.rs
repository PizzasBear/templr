use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::{
    parse::{Parse, ParseStream},
    Token,
};

#[derive(Debug, Clone)]
pub struct LetInit {
    pub eq_token: Token![=],
    pub expr: Box<syn::Expr>,
}

impl ToTokens for LetInit {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.eq_token.to_tokens(tokens);
        self.expr.to_tokens(tokens);
    }
}

#[derive(Debug, Clone)]
pub struct Let {
    pub pound: Token![#],
    pub let_token: Token![let],
    pub pat: Box<syn::Pat>,
    pub init: Option<LetInit>,
    pub semi: Token![;],
}

impl Parse for Let {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            pound: input.parse()?,
            let_token: input.parse()?,
            pat: {
                let pat = Box::new(syn::Pat::parse_single(input)?);
                match input.peek(Token![:]) {
                    true => Box::new(From::from(syn::PatType {
                        attrs: vec![],
                        pat,
                        colon_token: input.parse()?,
                        ty: Box::new(input.parse()?),
                    })),
                    false => pat,
                }
            },
            init: match input.parse()? {
                Some(eq_token) => Some(LetInit {
                    eq_token,
                    expr: input.parse()?,
                }),
                None => None,
            },
            semi: input.parse()?,
        })
    }
}

impl ToTokens for Let {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.pound.to_tokens(tokens);
        self.let_token.to_tokens(tokens);
        self.pat.to_tokens(tokens);
        self.init.to_tokens(tokens);
        self.semi.to_tokens(tokens);
    }
}
