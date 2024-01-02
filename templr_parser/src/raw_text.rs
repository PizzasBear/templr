use core::fmt;

use proc_macro2::{Span, TokenStream, TokenTree};
use quote::{ToTokens, TokenStreamExt};
use syn::{
    parse::{Parse, ParseStream},
    spanned::Spanned,
    token::Brace,
    Token,
};

#[derive(Debug, Clone)]
pub struct RawText {
    pub tokens: TokenStream,
}

impl Parse for RawText {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut tokens = TokenStream::new();
        while !input.is_empty()
            && !input.peek(Token![<])
            && !input.peek(Token![&])
            && !input.peek(Token![#])
            && !input.peek(Brace)
        {
            tokens.append(TokenTree::parse(input)?);
        }
        match tokens.is_empty() {
            true => Err(input.error("expected raw text")),
            false => Ok(Self { tokens }),
        }
    }
}

impl ToTokens for RawText {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.tokens.to_tokens(tokens);
    }
}

impl fmt::Display for RawText {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.join_spans().and_then(|span| span.source_text()) {
            Some(source) => fmt::Display::fmt(&source, f),
            None => fmt::Display::fmt(&self.tokens, f),
        }
    }
}

impl RawText {
    pub fn join_spans(&self) -> Option<Span> {
        let mut spans = self.tokens.clone().into_iter().map(|tt| tt.span());
        let first_span = spans.next()?.span();
        spans.try_fold(first_span, |sum, curr| sum.join(curr))
    }
}
