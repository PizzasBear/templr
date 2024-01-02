use proc_macro2::{extra::DelimSpan, TokenStream};
use quote::ToTokens;
use syn::{
    braced,
    parse::{Parse, ParseStream},
    token::{self, Brace},
    Token,
};

pub mod attrs;
pub mod call;
pub mod doctype;
pub mod element;
pub mod entity;
pub mod for_stmt;
pub mod if_stmt;
pub mod let_stmt;
pub mod match_stmt;
pub mod name;
pub mod raw_text;

pub use {
    attrs::Attr, call::Call, doctype::Doctype, element::Element, entity::Entity, for_stmt::For,
    if_stmt::If, let_stmt::Let, match_stmt::Match, name::Name, raw_text::RawText,
};

/// Parses the body of a hash statement, consumes the entirery of its input
fn parse_to_vec<T: Parse>(input: ParseStream) -> syn::Result<Vec<T>> {
    let mut body = vec![];
    while !input.is_empty() {
        body.push(T::parse(input)?);
    }
    Ok(body)
}

#[derive(Debug, Clone)]
pub struct PoundBlock<T> {
    pub pound: token::Pound,
    pub brace: Brace,
    pub body: Vec<T>,
}

impl<T: Parse> Parse for PoundBlock<T> {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        Ok(Self {
            pound: input.parse()?,
            brace: braced!(content in input),
            body: parse_to_vec(&content)?,
        })
    }
}

impl<T: ToTokens> ToTokens for PoundBlock<T> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.pound.to_tokens(tokens);
        self.brace.surround(tokens, |tokens| {
            for item in &self.body {
                item.to_tokens(tokens)
            }
        });
    }
}

#[derive(Debug, Clone)]
pub enum Block {
    Valid(syn::Block),
    Invalid { brace: Brace, body: TokenStream },
}

impl Block {
    pub fn brace_span(&self) -> DelimSpan {
        match self {
            Block::Valid(block) => block.brace_token.span,
            Block::Invalid { brace, .. } => brace.span,
        }
    }
}

impl Parse for Block {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        match input.parse() {
            Ok(block) => Ok(Self::Valid(block)),
            Err(_) => Ok(Self::Invalid {
                brace: braced!(content in input),
                body: content.parse()?,
            }),
        }
    }
}

impl ToTokens for Block {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Valid(block) => block.to_tokens(tokens),
            Self::Invalid { brace, body } => {
                brace.surround(tokens, |tokens| body.to_tokens(tokens))
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Node {
    Entity(Entity),
    Doctype(Doctype),
    Element(Element),
    RawText(RawText),
    Expr(Block),
    If(If<Self>),
    Match(Match<Self>),
    For(For<Self>),
    Block(PoundBlock<Self>),
    Let(Let),
    Call(Call),
}

impl Parse for Node {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(Token![&]) {
            Ok(Self::Entity(input.parse()?))
        } else if input.peek(Token![<]) {
            if input.peek2(Token![!]) {
                Ok(Self::Doctype(input.parse()?))
            } else {
                Ok(Self::Element(input.parse()?))
            }
        } else if input.peek(Brace) {
            Ok(Self::Expr(input.parse()?))
        } else if input.peek(Token![#]) {
            if input.peek2(Token![if]) {
                Ok(Self::If(input.parse()?))
            } else if input.peek2(Token![match]) {
                Ok(Self::Match(input.parse()?))
            } else if input.peek2(Token![for]) {
                Ok(Self::For(input.parse()?))
            } else if input.peek2(Token![let]) {
                Ok(Self::Let(input.parse()?))
            } else if input.peek2(Brace) {
                Ok(Self::Block(input.parse()?))
            } else {
                Ok(Self::Call(input.parse()?))
            }
        } else {
            Ok(Self::RawText(input.parse()?))
        }
    }
}

impl ToTokens for Node {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Entity(slf) => slf.to_tokens(tokens),
            Self::Doctype(slf) => slf.to_tokens(tokens),
            Self::Element(slf) => slf.to_tokens(tokens),
            Self::RawText(slf) => slf.to_tokens(tokens),
            Self::Expr(slf) => slf.to_tokens(tokens),
            Self::If(slf) => slf.to_tokens(tokens),
            Self::Match(slf) => slf.to_tokens(tokens),
            Self::For(slf) => slf.to_tokens(tokens),
            Self::Block(slf) => slf.to_tokens(tokens),
            Self::Let(slf) => slf.to_tokens(tokens),
            Self::Call(slf) => slf.to_tokens(tokens),
        }
    }
}

mod kw {
    syn::custom_keyword!(context);
    syn::custom_keyword!(children);
}

#[derive(Debug, Clone)]
pub struct UseContext {
    pub pound: Token![#],
    pub use_token: Token![use],
    pub context: kw::context,
    pub as_pat: Option<(Token![as], Box<syn::Pat>)>,
    pub colon_ty: Option<(Token![:], Token![&], Box<syn::Type>)>,
    pub semi: Token![;],
}

impl Parse for UseContext {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            pound: input.parse()?,
            use_token: input.parse()?,
            context: input.parse()?,
            as_pat: {
                let lookahead1 = input.lookahead1();
                match lookahead1.peek(Token![as]) {
                    true => Some((input.parse()?, Box::new(syn::Pat::parse_single(input)?))),
                    false if lookahead1.peek(Token![:]) || lookahead1.peek(Token![;]) => None,
                    false => return Err(lookahead1.error()),
                }
            },
            colon_ty: {
                let lookahead1 = input.lookahead1();
                match lookahead1.peek(Token![:]) {
                    true => Some((input.parse()?, input.parse()?, Box::new(input.parse()?))),
                    false if lookahead1.peek(Token![;]) => None,
                    false => return Err(lookahead1.error()),
                }
            },
            semi: input.parse()?,
        })
    }
}

impl ToTokens for UseContext {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.pound.to_tokens(tokens);
        self.use_token.to_tokens(tokens);
        self.context.to_tokens(tokens);
        if let Some((as_token, pat)) = &self.as_pat {
            as_token.to_tokens(tokens);
            pat.to_tokens(tokens);
        }
        if let Some((colon, amp, ty)) = &self.colon_ty {
            colon.to_tokens(tokens);
            amp.to_tokens(tokens);
            ty.to_tokens(tokens);
        }
        self.semi.to_tokens(tokens);
    }
}

#[derive(Debug, Clone)]
pub struct UseChildren {
    pub pound: Token![#],
    pub use_token: Token![use],
    pub children: kw::children,
    pub as_pat: Option<(Token![as], Box<syn::Pat>)>,
    pub semi: Token![;],
}

impl Parse for UseChildren {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            pound: input.parse()?,
            use_token: input.parse()?,
            children: input.parse()?,
            as_pat: {
                let lookahead1 = input.lookahead1();
                match lookahead1.peek(Token![as]) {
                    true => Some((input.parse()?, Box::new(syn::Pat::parse_single(input)?))),
                    false if lookahead1.peek(Token![;]) => None,
                    false => return Err(lookahead1.error()),
                }
            },
            semi: input.parse()?,
        })
    }
}

impl ToTokens for UseChildren {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.pound.to_tokens(tokens);
        self.use_token.to_tokens(tokens);
        self.children.to_tokens(tokens);
        if let Some((as_token, pat)) = &self.as_pat {
            as_token.to_tokens(tokens);
            pat.to_tokens(tokens);
        }
        self.semi.to_tokens(tokens);
    }
}

#[derive(Debug, Clone)]
pub struct TemplBody {
    pub use_context: Option<UseContext>,
    pub use_children: Option<UseChildren>,
    pub nodes: Vec<Node>,
}

impl Parse for TemplBody {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut use_context = None;
        let mut use_children = None;

        while input.peek(Token![#]) && input.peek2(Token![use]) {
            let fork = input.fork();
            let _: Token![#] = fork.parse()?;
            let _: Token![use] = fork.parse()?;
            let lookahead1 = fork.lookahead1();
            if lookahead1.peek(kw::children) {
                if use_children.is_some() {
                    return Err(input.error("Cannot redefine `#use children ...`"));
                }
                use_children = Some(input.parse()?);
            } else if lookahead1.peek(kw::context) {
                if use_context.is_some() {
                    return Err(input.error("Cannot redefine `#use context ...`"));
                }
                use_context = Some(input.parse()?);
            } else {
                return Err(lookahead1.error());
            }
        }

        Ok(Self {
            use_context,
            use_children,
            nodes: parse_to_vec(input)?,
        })
    }
}

impl ToTokens for TemplBody {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.use_context.to_tokens(tokens);
        for node in &self.nodes {
            node.to_tokens(tokens);
        }
    }
}
