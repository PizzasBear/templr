use proc_macro2::TokenStream;
use quote::{ToTokens, TokenStreamExt};
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
            name: Name::parse_tag(input)?,
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
        tokens.append_all(&self.attrs);
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
            name: Name::parse_tag(input)?,
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

impl Element {
    #[inline]
    pub const fn name(&self) -> &Name {
        &self.open.name
    }
}

impl Parse for Element {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let open = OpenTag::parse(input)?;
        let open_name = open.name.to_string();
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
                        if input.is_empty() {
                            return Err(
                                input.error(format!("expected closing tag: `</{open_name}>`"))
                            );
                        }
                        nodes.push(Node::parse(input)?);
                    }
                    nodes
                },
                close: {
                    let close: CloseTag = input.parse()?;
                    if close.name.to_string() != open_name {
                        return Err(syn::Error::new_spanned(
                            close.name,
                            format!("Mismatched closing tag, expected: `</{open_name}/>`"),
                        ));
                    }
                    Some(close)
                },
            }),
        }
    }
}

impl ToTokens for Element {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.open.to_tokens(tokens);
        tokens.append_all(&self.nodes);
        self.close.to_tokens(tokens);
    }
}
