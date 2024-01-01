use core::fmt;

use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::{
    parse::{Parse, ParseStream},
    Token,
};

use crate::Name;

pub mod kw {
    syn::custom_keyword!(DOCTYPE);
}

#[derive(Debug, Clone)]
pub struct Doctype {
    pub lt: Token![<],
    pub bang: Token![!],
    pub doctype: kw::DOCTYPE,
    pub name: Name,
    pub gt: Token![>],
}

impl Parse for Doctype {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            lt: input.parse()?,
            bang: input.parse()?,
            doctype: input.parse()?,
            name: Name::parse_tag(input)?,
            gt: input.parse()?,
        })
    }
}

impl ToTokens for Doctype {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.lt.to_tokens(tokens);
        self.bang.to_tokens(tokens);
        self.doctype.to_tokens(tokens);
        self.name.to_tokens(tokens);
        self.gt.to_tokens(tokens);
    }
}

impl fmt::Display for Doctype {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<!DOCTYPE {}>", self.name)
    }
}
