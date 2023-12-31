use proc_macro2::{TokenStream, TokenTree};
use quote::{ToTokens, TokenStreamExt};
use syn::{
    parse::{Parse, ParseStream},
    token::Brace,
    Token,
};

#[derive(Debug, Clone)]
pub struct RawText {
    pub tokens: TokenStream,
}

impl RawText {
    pub fn to_string(&self) -> String {
        let mut tokens = self.tokens.clone().into_iter();
        let Some(mut span) = tokens.next().map(|tt| tt.span()) else {
            return self.tokens.to_string();
        };
        for tt in tokens {
            let Some(joined) = span.join(tt.span()) else {
                return self.tokens.to_string();
            };
            span = joined;
        }
        span.source_text()
            .unwrap_or_else(|| self.tokens.to_string())
    }
}

impl Parse for RawText {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut tokens = TokenStream::new();
        while !input.peek(Token![<])
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
