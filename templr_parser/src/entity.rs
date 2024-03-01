use core::fmt;

use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::{
    ext::*,
    parse::{Parse, ParseStream},
    Ident, LitInt, Token,
};

// Maybe validate entities using `https://html.spec.whatwg.org/entities.json`
#[derive(Debug, Clone)]
pub struct NamedEntity {
    pub amp: Token![&],
    pub ident: Ident,
    pub semicolon: Token![;],
}

impl Parse for NamedEntity {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            amp: input.parse()?,
            ident: Ident::parse_any(input)?,
            semicolon: input.parse()?,
        })
    }
}

impl ToTokens for NamedEntity {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.amp.to_tokens(tokens);
        self.ident.to_tokens(tokens);
        self.semicolon.to_tokens(tokens);
    }
}

impl fmt::Display for NamedEntity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "&{};", self.ident)
    }
}

#[derive(Debug, Clone)]
pub struct DecimalEntity {
    pub amp: Token![&],
    pub pound: Token![#],
    pub decimal: syn::Index,
    pub semicolon: Token![;],
}

impl Parse for DecimalEntity {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            amp: input.parse()?,
            pound: input.parse()?,
            decimal: input.parse()?,
            semicolon: input.parse()?,
        })
    }
}
impl ToTokens for DecimalEntity {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.amp.to_tokens(tokens);
        self.pound.to_tokens(tokens);
        self.decimal.to_tokens(tokens);
        self.semicolon.to_tokens(tokens);
    }
}

impl fmt::Display for DecimalEntity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "&#{};", self.decimal.index)
    }
}

#[derive(Debug, Clone)]
pub struct HexEntity {
    pub amp: Token![&],
    pub pound: Token![#],
    pub hex: Ident,
    pub semicolon: Token![;],
}

impl Parse for HexEntity {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            amp: input.parse()?,
            pound: input.parse()?,
            hex: {
                let ident: Ident = input.parse()?;
                let s = ident.to_string();
                if !s.starts_with('x') || !s[1..].bytes().all(|b| b.is_ascii_hexdigit()) {
                    return Err(syn::Error::new_spanned(ident, "invalid entity code"));
                }
                ident
            },
            semicolon: input.parse()?,
        })
    }
}

impl ToTokens for HexEntity {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.amp.to_tokens(tokens);
        self.pound.to_tokens(tokens);
        self.hex.to_tokens(tokens);
        self.semicolon.to_tokens(tokens);
    }
}

impl fmt::Display for HexEntity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "&#{};", self.hex)
    }
}

#[derive(Debug, Clone)]
pub enum Entity {
    Named(NamedEntity),
    Decimal(DecimalEntity),
    Hex(HexEntity),
}

impl Entity {
    pub fn amp(&self) -> Token![&] {
        match self {
            Entity::Named(entity) => entity.amp,
            Entity::Decimal(entity) => entity.amp,
            Entity::Hex(entity) => entity.amp,
        }
    }
}

impl Parse for Entity {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek2(Ident::peek_any) {
            Ok(Self::Named(input.parse()?))
        } else if input.peek3(Ident) {
            Ok(Self::Hex(input.parse()?))
        } else if input.peek3(LitInt) {
            Ok(Self::Decimal(input.parse()?))
        } else {
            Err(input.error("expected an HTML entity (`&apos;`, `&#x27;`, or `&#39;`)"))
        }
    }
}

impl ToTokens for Entity {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Named(slf) => slf.to_tokens(tokens),
            Self::Decimal(slf) => slf.to_tokens(tokens),
            Self::Hex(slf) => slf.to_tokens(tokens),
        }
    }
}

impl fmt::Display for Entity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Named(slf) => fmt::Display::fmt(slf, f),
            Self::Decimal(slf) => fmt::Display::fmt(slf, f),
            Self::Hex(slf) => fmt::Display::fmt(slf, f),
        }
    }
}
