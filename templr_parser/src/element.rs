use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::{
    parse::{Parse, ParseStream},
    Token,
};

use crate::{Attr, Name, Node};

#[derive(Debug, Clone)]
pub struct OpenTag {
    pub lt: Token![<],
    pub name: Name,
    pub attrs: Vec<Attr>,
    pub slash: Option<Token![/]>,
    pub gt: Token![>],
}

impl Parse for OpenTag {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            lt: input.parse()?,
            name: input.parse()?,
            attrs: {
                let mut attrs = vec![];
                while !input.peek(Token![/]) && !input.peek(Token![>]) {
                    attrs.push(Attr::parse(input)?);
                }
                attrs
            },
            slash: input.parse()?,
            gt: input.parse()?,
        })
    }
}

impl ToTokens for OpenTag {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.lt.to_tokens(tokens);
        self.name.to_tokens(tokens);
        for attr in &self.attrs {
            attr.to_tokens(tokens);
        }
        self.slash.to_tokens(tokens);
        self.gt.to_tokens(tokens);
    }
}

#[derive(Debug, Clone)]
pub struct CloseTag {
    pub lt: Token![<],
    pub slash: Token![/],
    pub name: Name,
    pub gt: Token![>],
}

impl Parse for CloseTag {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            lt: input.parse()?,
            slash: input.parse()?,
            name: input.parse()?,
            gt: input.parse()?,
        })
    }
}

impl ToTokens for CloseTag {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.lt.to_tokens(tokens);
        self.slash.to_tokens(tokens);
        self.name.to_tokens(tokens);
        self.gt.to_tokens(tokens);
    }
}

#[derive(Debug, Clone)]
pub struct Element {
    pub open: OpenTag,
    pub nodes: Vec<Node>,
    pub close: Option<CloseTag>,
}

impl Parse for Element {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let open = OpenTag::parse(input)?;
        match open.slash {
            Some(_) => Ok(Self {
                open,
                nodes: vec![],
                close: None,
            }),
            None => Ok(Self {
                open,
                nodes: {
                    let mut nodes = vec![];
                    while !input.peek(Token![<]) || !input.peek2(Token![/]) {
                        nodes.push(Node::parse(input)?);
                    }
                    nodes
                },
                close: Some(input.parse()?),
            }),
        }
    }
}

impl ToTokens for Element {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.open.to_tokens(tokens);
        for node in &self.nodes {
            node.to_tokens(tokens);
        }
        self.close.to_tokens(tokens);
    }
}
