use proc_macro2::{extra::DelimSpan, TokenStream};
use quote::{ToTokens, TokenStreamExt};
use syn::{
    braced, bracketed, parenthesized,
    parse::{Parse, ParseStream},
    token::{self, Brace, Bracket, Group, Paren},
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

mod kw {
    syn::custom_keyword!(context);
    syn::custom_keyword!(children);
    syn::custom_keyword!(with);
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
pub struct Scope<T> {
    pub pound: token::Pound,
    pub brace: Brace,
    pub body: Vec<T>,
}

impl<T: Parse> Parse for Scope<T> {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        Ok(Self {
            pound: input.parse()?,
            brace: braced!(content in input),
            body: parse_to_vec(&content)?,
        })
    }
}

impl<T: ToTokens> ToTokens for Scope<T> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.pound.to_tokens(tokens);
        self.brace.surround(tokens, |tokens| {
            tokens.append_all(&self.body);
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
pub struct With {
    pub pound: Token![#],
    pub with: kw::with,
    pub context: kw::context,
    pub ty: Option<(Token![:], Box<syn::Type>)>,
    pub eq: Token![=],
    pub expr: Box<syn::Expr>,
    pub brace: Brace,
    pub nodes: Vec<Node>,
}

impl Parse for With {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        Ok(Self {
            pound: input.parse()?,
            with: input.parse()?,
            context: input.parse()?,
            ty: {
                let lookahead1 = input.lookahead1();
                if lookahead1.peek(Token![:]) {
                    Some((
                        input.parse()?,
                        Box::new(From::from(syn::TypeReference {
                            and_token: input.parse()?,
                            lifetime: None,
                            mutability: None,
                            elem: input.parse()?,
                        })),
                    ))
                } else if lookahead1.peek(Token![=]) {
                    None
                } else {
                    return Err(lookahead1.error());
                }
            },
            eq: input.parse()?,
            expr: Box::new(syn::Expr::parse_without_eager_brace(input)?),
            brace: braced!(content in input),
            nodes: parse_to_vec(&content)?,
        })
    }
}

impl ToTokens for With {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.pound.to_tokens(tokens);
        self.with.to_tokens(tokens);
        self.context.to_tokens(tokens);
        if let Some((colon, ty)) = &self.ty {
            colon.to_tokens(tokens);
            ty.to_tokens(tokens);
        }
        self.eq.to_tokens(tokens);
        self.expr.to_tokens(tokens);
        self.brace.surround(tokens, |tokens| {
            tokens.append_all(&self.nodes);
        });
    }
}

// #use context as ctx: &u32;
// #with context: &u32 = &0 {
//     something
// }

#[derive(Debug, Clone)]
pub enum Node {
    Entity(Entity),
    Doctype(Doctype),
    Element(Element),
    RawText(RawText),
    Paren(Paren, Vec<Node>),
    Bracket(Bracket, Vec<Node>),
    Expr(Block),
    If(If<Self>),
    Match(Match<Self>),
    For(For<Self>),
    Scope(Scope<Self>),
    Let(Let),
    Call(Call),
    With(With),
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
                Ok(Self::Scope(input.parse()?))
            } else if input.peek2(kw::with) {
                Ok(Self::With(input.parse()?))
            } else {
                Ok(Self::Call(input.parse()?))
            }
        } else if input.peek(Paren) {
            let content;
            Ok(Self::Paren(
                parenthesized!(content in input),
                parse_to_vec(&content)?,
            ))
        } else if input.peek(Bracket) {
            let content;
            Ok(Self::Bracket(
                bracketed!(content in input),
                parse_to_vec(&content)?,
            ))
        } else if input.peek(Group) {
            Err(input.error("unexpected none deliminated group"))
        } else if input.is_empty() {
            Err(input.error("expected a templr node"))
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
            Self::Paren(paren, nodes) => paren.surround(tokens, |tokens| {
                tokens.append_all(nodes);
            }),
            Self::Bracket(bracket, nodes) => bracket.surround(tokens, |tokens| {
                tokens.append_all(nodes);
            }),
            Self::Expr(slf) => slf.to_tokens(tokens),
            Self::If(slf) => slf.to_tokens(tokens),
            Self::Match(slf) => slf.to_tokens(tokens),
            Self::For(slf) => slf.to_tokens(tokens),
            Self::Scope(slf) => slf.to_tokens(tokens),
            Self::Let(slf) => slf.to_tokens(tokens),
            Self::Call(slf) => slf.to_tokens(tokens),
            Self::With(slf) => slf.to_tokens(tokens),
        }
    }
}

#[derive(Debug, Clone)]
pub struct UseContext {
    pub pound: Token![#],
    pub use_token: Token![use],
    pub context: kw::context,
    pub as_pat: Option<(Token![as], Box<syn::Pat>)>,
    pub colon_ty: Option<(Token![:], Box<syn::Type>)>,
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
                    true => Some((
                        input.parse()?,
                        Box::new(From::from(syn::TypeReference {
                            and_token: input.parse()?,
                            lifetime: None,
                            mutability: None,
                            elem: input.parse()?,
                        })),
                    )),
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
        if let Some((colon, ty)) = &self.colon_ty {
            colon.to_tokens(tokens);
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
pub enum Use {
    Context(UseContext),
    Children(UseChildren),
}

impl Parse for Use {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let fork = input.fork();
        let _: Token![#] = fork.parse()?;
        let _: Token![use] = fork.parse()?;
        let lookahead1 = fork.lookahead1();
        if lookahead1.peek(kw::children) {
            Ok(Use::Children(input.parse()?))
        } else if lookahead1.peek(kw::context) {
            Ok(Use::Context(input.parse()?))
        } else {
            Err(lookahead1.error())
        }
    }
}

impl ToTokens for Use {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Context(slf) => slf.to_tokens(tokens),
            Self::Children(slf) => slf.to_tokens(tokens),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TemplBody {
    pub uses: Vec<Use>,
    pub nodes: Vec<Node>,
}

impl TemplBody {
    pub fn use_children(&self) -> Option<&UseChildren> {
        self.uses.iter().find_map(|u| match u {
            Use::Children(u) => Some(u),
            _ => None,
        })
    }
    pub fn use_context(&self) -> Option<&UseContext> {
        self.uses.iter().find_map(|u| match u {
            Use::Context(u) => Some(u),
            _ => None,
        })
    }
}

impl Parse for TemplBody {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut uses = vec![];

        while input.peek(Token![#]) && input.peek2(Token![use]) {
            uses.push(Use::parse(input)?);
        }
        if let Some(u) = uses
            .iter()
            .filter_map(|u| match u {
                Use::Children(u) => Some(u),
                _ => None,
            })
            .nth(1)
        {
            return Err(syn::Error::new(
                u.pound.span,
                "Cannot redefine `#use children ...`",
            ));
        } else if let Some(u) = uses
            .iter()
            .filter_map(|u| match u {
                Use::Context(u) => Some(u),
                _ => None,
            })
            .nth(1)
        {
            return Err(syn::Error::new(
                u.pound.span,
                "Cannot redefine `#use context ...`",
            ));
        }

        Ok(Self {
            uses,
            nodes: parse_to_vec(input)?,
        })
    }
}

impl ToTokens for TemplBody {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.append_all(&self.uses);
        tokens.append_all(&self.nodes);
    }
}
