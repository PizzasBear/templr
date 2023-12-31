use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::{
    braced,
    parse::{Parse, ParseStream},
    token::{self, Brace},
    Token,
};

pub mod attrs;
pub mod element;
pub mod entity;
pub mod name;
pub mod raw_text;

pub use attrs::Attr;
pub use element::Element;
pub use entity::Entity;
pub use name::Name;
pub use raw_text::RawText;

macro_rules! ignore {
    ($($_:tt)*) => {};
}

ignore! {
    <hr
        style="padding: 10px"
        #if true {
            class="itIsTrue"
        }
        {..[("hello", "world"), ("sure", "so it's enough")]}
    />
    #match opt {
        Some(x) => {
            <as>
        }
        None => {
            <asd>
        }
    }
}

/// Parses the body of a hash statement, consumes the entirery of its input
fn parse_to_vec<T: Parse>(input: ParseStream) -> syn::Result<Vec<T>> {
    let mut body = vec![];
    while !input.is_empty() {
        body.push(T::parse(input)?);
    }
    Ok(body)
}

#[derive(Debug, Clone)]
pub struct ElseIfBranch<T> {
    pub else_token: token::Else,
    pub if_token: token::If,
    pub cond: Box<syn::Expr>,
    pub brace: Brace,
    pub body: Vec<T>,
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

#[derive(Debug, Clone)]
pub struct ElseBranch<T> {
    pub else_token: token::Else,
    pub brace: Brace,
    pub body: Vec<T>,
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
        if let Some((if_token, expr)) = self.guard {
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
            for item in &self.body {
                item.to_tokens(tokens)
            }
        });
    }
}

#[derive(Debug, Clone)]
pub enum Node {
    Entity(Entity),
    Element(Element),
    RawText(RawText),
    Expr(syn::Block),
    If(If<Self>),
    Match(Match<Self>),
    For(For<Self>),
}

impl Parse for Node {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(Token![&]) {
            Ok(Self::Entity(input.parse()?))
        } else if input.peek(Token![<]) {
            Ok(Self::Element(input.parse()?))
        } else if input.peek(Brace) {
            Ok(Self::Expr(input.parse()?))
        } else if input.peek(Token![#]) {
            let fork = input.fork();
            let _: Token![#] = fork.parse()?;

            let lookahead1 = fork.lookahead1();
            if lookahead1.peek(Token![if]) {
                Ok(Self::If(input.parse()?))
            } else if lookahead1.peek(Token![match]) {
                Ok(Self::Match(input.parse()?))
            } else if lookahead1.peek(Token![for]) {
                Ok(Self::For(input.parse()?))
            } else {
                Err(lookahead1.error())
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
            Self::Element(slf) => slf.to_tokens(tokens),
            Self::RawText(slf) => slf.to_tokens(tokens),
            Self::Expr(slf) => slf.to_tokens(tokens),
            Self::If(slf) => slf.to_tokens(tokens),
            Self::Match(slf) => slf.to_tokens(tokens),
            Self::For(slf) => slf.to_tokens(tokens),
        }
    }
}

#[derive(Debug, Clone)]
pub struct NodeBody {
    pub nodes: Vec<Node>,
}

impl Parse for NodeBody {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut nodes = vec![];
        while !input.is_empty() {
            nodes.push(Node::parse(input)?);
        }
        Ok(Self { nodes })
    }
}

impl ToTokens for NodeBody {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for node in &self.nodes {
            node.to_tokens(tokens);
        }
    }
}
