use std::{
    borrow::Cow,
    fmt::{self, Write},
    marker::PhantomData,
    ops,
};

use askama_escape::Escaper;
use parser::Element;
use proc_macro2::{Delimiter, Span, TokenStream, TokenTree};
use quote::{quote, quote_spanned, ToTokens, TokenStreamExt};
use syn::{parse_quote, punctuated::Punctuated, Ident, Token};
use templr_parser::{self as parser, Node, TemplBody};

const SELF_CLOSING: &[&str] = &[
    "arena", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "param", "source",
    "track", "wbr", // html
];

#[rustfmt::skip]
fn can_attrs_break(attrs: &[syn::Attribute]) -> bool {
    !attrs.iter().all(|attr| {
        attr.path().get_ident().is_some_and(|ident| {
            [
                // Conditional compilation
                "cfg", "cfg_attr",
                // Testing
                "test", "ignore", "should_panic",
                // Derive
                "derive", "automatically_derived",
                // Macros
                "macro_export", "macro_use", "proc_macro", "proc_macro_derive",
                "proc_macro_attribute",
                // Diagnostics
                "allow", "warn", "deny", "forbid", "deprecated", "must_use",
                // ABI, linking, symbols, and FFI
                "link", "link_name", "link_ordinal", "no_link", "repr", "crate_type",
                "no_main", "export_name", "link_section", "no_mangle", "used", "crate_name",
                // Code generation
                "inline", "cold", "no_builtins", "target_feature", "track_caller",
                "instruction_set",
                // Documentation
                "doc",
                // Preludes
                "no_std", "no_implicit_prelude",
                // Modules
                "path",
                // Limits
                "recursion_limit", "type_length_limit",
                // Runtime
                "panic_handler", "global_allocator", "windows_subsystem",
                // Features
                "feature",
                // Type System
                "non_exhaustive",
                // Debugger
                "debugger_visualizer",
            ]
            .iter()
            .any(|s| ident == s)
        })
    })
}

fn can_macro_break(mac: &syn::Macro) -> bool {
    !mac.path.get_ident().is_some_and(|ident| {
        // returns true if can't break;

        if ["cfg", "stringify", "concat", "include_str", "include_bytes"]
            .iter()
            .any(|s| ident == s)
        {
            true
        } else if [
            "todo",
            "unreachable",
            "unimplemented",
            "panic",
            "assert",
            "assert_eq",
            "assert_ne",
            "debug_assert",
            "debug_assert_eq",
            "debug_assert_ne",
            "dbg",
            "print",
            "println",
            "write",
            "writeln",
            "format",
            "format_args",
        ]
        .iter()
        .any(|s| ident == s)
        {
            mac.parse_body_with(Punctuated::<syn::Expr, Token![,]>::parse_terminated)
                .ok()
                .map_or(false, |exprs| !exprs.iter().any(can_expr_break))
        } else {
            false
        }
    })
}

fn can_expr_break(expr: &syn::Expr) -> bool {
    match expr {
        syn::Expr::Array(expr) => {
            can_attrs_break(&expr.attrs) || expr.elems.iter().any(can_expr_break)
        }
        syn::Expr::Assign(expr) => {
            can_attrs_break(&expr.attrs)
                || can_expr_break(&expr.left)
                || can_expr_break(&expr.right)
        }
        syn::Expr::Async(_) => false,
        syn::Expr::Await(expr) => can_attrs_break(&expr.attrs) || can_expr_break(&expr.base),
        syn::Expr::Binary(expr) => {
            can_attrs_break(&expr.attrs)
                || can_expr_break(&expr.left)
                || can_expr_break(&expr.right)
        }
        syn::Expr::Block(expr) => can_attrs_break(&expr.attrs) || can_block_break(&expr.block),
        syn::Expr::Break(expr) => {
            can_attrs_break(&expr.attrs)
                || expr.label.is_none()
                || expr.expr.as_ref().is_some_and(|expr| can_expr_break(expr))
        }
        syn::Expr::Call(expr) => {
            can_attrs_break(&expr.attrs)
                || can_expr_break(&expr.func)
                || expr.args.iter().any(can_expr_break)
        }
        syn::Expr::Cast(expr) => can_attrs_break(&expr.attrs) || can_expr_break(&expr.expr),
        syn::Expr::Closure(expr) => can_attrs_break(&expr.attrs),
        syn::Expr::Const(expr) => can_attrs_break(&expr.attrs) || can_block_break(&expr.block),
        syn::Expr::Continue(expr) => can_attrs_break(&expr.attrs) || expr.label.is_none(),
        syn::Expr::Field(expr) => can_attrs_break(&expr.attrs) || can_expr_break(&expr.base),
        syn::Expr::ForLoop(expr) => can_attrs_break(&expr.attrs),
        syn::Expr::Group(expr) => can_attrs_break(&expr.attrs) || can_expr_break(&expr.expr),
        syn::Expr::If(expr) => {
            can_attrs_break(&expr.attrs)
                || can_expr_break(&expr.cond)
                || can_block_break(&expr.then_branch)
                || expr
                    .else_branch
                    .as_ref()
                    .is_some_and(|(_, expr)| can_expr_break(expr))
        }
        syn::Expr::Index(expr) => {
            can_attrs_break(&expr.attrs)
                || can_expr_break(&expr.expr)
                || can_expr_break(&expr.index)
        }
        syn::Expr::Infer(_) => false,
        syn::Expr::Let(expr) => can_attrs_break(&expr.attrs) || can_expr_break(&expr.expr),
        syn::Expr::Lit(expr) => can_attrs_break(&expr.attrs),
        syn::Expr::Loop(expr) => can_attrs_break(&expr.attrs),
        syn::Expr::Macro(syn::ExprMacro { attrs, mac }) => {
            can_attrs_break(attrs) || can_macro_break(mac)
        }
        syn::Expr::Match(expr) => {
            can_attrs_break(&expr.attrs)
                || can_expr_break(&expr.expr)
                || expr.arms.iter().any(|arm| {
                    !arm.attrs.is_empty()
                        || arm
                            .guard
                            .as_ref()
                            .is_some_and(|(_, expr)| can_expr_break(expr))
                        || can_expr_break(&expr.expr)
                })
        }
        syn::Expr::MethodCall(expr) => {
            can_attrs_break(&expr.attrs)
                || can_expr_break(&expr.receiver)
                || expr.args.iter().any(can_expr_break)
        }
        syn::Expr::Paren(expr) => can_attrs_break(&expr.attrs) || can_expr_break(&expr.expr),
        syn::Expr::Path(expr) => can_attrs_break(&expr.attrs),
        syn::Expr::Range(expr) => {
            can_attrs_break(&expr.attrs)
                || expr.start.as_ref().is_some_and(|expr| can_expr_break(expr))
                || expr.end.as_ref().is_some_and(|expr| can_expr_break(expr))
        }
        syn::Expr::Reference(expr) => can_attrs_break(&expr.attrs) || can_expr_break(&expr.expr),
        syn::Expr::Repeat(expr) => can_attrs_break(&expr.attrs) || can_expr_break(&expr.expr),
        syn::Expr::Return(expr) => {
            can_attrs_break(&expr.attrs)
                || expr.expr.as_ref().is_some_and(|expr| can_expr_break(expr))
        }
        syn::Expr::Struct(expr) => {
            can_attrs_break(&expr.attrs)
                || expr.fields.iter().any(|expr| can_expr_break(&expr.expr))
        }
        syn::Expr::Try(expr) => can_attrs_break(&expr.attrs) || can_expr_break(&expr.expr),
        syn::Expr::TryBlock(expr) => can_attrs_break(&expr.attrs) || can_block_break(&expr.block),
        syn::Expr::Tuple(expr) => {
            can_attrs_break(&expr.attrs) || expr.elems.iter().any(can_expr_break)
        }
        syn::Expr::Unary(expr) => can_attrs_break(&expr.attrs) || can_expr_break(&expr.expr),
        syn::Expr::Unsafe(expr) => can_attrs_break(&expr.attrs) || can_block_break(&expr.block),
        syn::Expr::Verbatim(_) => true,
        syn::Expr::While(expr) => can_attrs_break(&expr.attrs) || can_expr_break(&expr.cond),
        syn::Expr::Yield(expr) => {
            can_attrs_break(&expr.attrs)
                || expr.expr.as_ref().is_some_and(|expr| can_expr_break(expr))
        }
        _ => true,
    }
}

fn can_block_break(block: &syn::Block) -> bool {
    block.stmts.iter().any(|stmt| match stmt {
        syn::Stmt::Item(_) => false,
        syn::Stmt::Local(syn::Local { attrs, init, .. }) => {
            can_attrs_break(attrs)
                || init
                    .as_ref()
                    .is_some_and(|syn::LocalInit { expr, diverge, .. }| {
                        can_expr_break(expr)
                            || diverge
                                .as_ref()
                                .is_some_and(|(_, expr)| can_expr_break(expr))
                    })
        }
        syn::Stmt::Macro(syn::StmtMacro { attrs, mac, .. }) => {
            can_attrs_break(attrs) || can_macro_break(mac)
        }
        syn::Stmt::Expr(expr, _) => can_expr_break(expr),
    })
}

fn try_stringify_expr(expr: &syn::Expr, can_break: bool) -> Option<(Span, String)> {
    if can_break && can_expr_break(expr) {
        return None;
    }
    match expr {
        syn::Expr::Lit(syn::ExprLit { attrs, lit }) if attrs.is_empty() => match lit {
            syn::Lit::Str(s) => Some((s.span(), s.value())),
            syn::Lit::Byte(byte) => Some((byte.span(), byte.value().to_string())),
            syn::Lit::Char(ch) => Some((ch.span(), ch.value().to_string())),
            syn::Lit::Int(int) => Some((int.span(), int.to_string())),
            syn::Lit::Float(float) => Some((float.span(), float.to_string())),
            syn::Lit::Bool(bool) => Some((bool.span(), bool.value().to_string())),
            _ => None,
        },
        syn::Expr::Paren(syn::ExprParen { expr, attrs, .. }) if attrs.is_empty() => {
            try_stringify_expr(expr, false)
        }
        syn::Expr::Block(syn::ExprBlock {
            block,
            attrs,
            label: None,
            ..
        }) if attrs.is_empty() => try_stringify_block(block, false),
        _ => None,
    }
}

fn try_stringify_block(block: &syn::Block, can_break: bool) -> Option<(Span, String)> {
    if can_break && can_block_break(block) {
        return None;
    }
    let Some(syn::Stmt::Expr(expr, None)) = block.stmts.iter().rfind(|stmt| {
        matches!(
            stmt,
            syn::Stmt::Expr(_, _) | syn::Stmt::Macro(_) | syn::Stmt::Local(_),
        )
    }) else {
        return None;
    };
    try_stringify_expr(expr, false)
}

fn writer() -> Ident {
    Ident::new("__templr_writer", Span::mixed_site())
}

fn context() -> Ident {
    Ident::new("__templr_ctx", Span::mixed_site())
}

fn crate_path(span: Span) -> syn::Path {
    syn::parse_quote_spanned!(span => ::templr)
}

/// Try to isolate `tokens` from `break`.
fn isolate_block(tokens: impl ToTokens) -> TokenStream {
    let mut iter = tokens.into_token_stream().into_iter();
    let Some(first) = iter.next() else {
        if cfg!(debug_assertions) {
            eprintln!("something weird happened")
        }
        return quote! { () };
    };

    let first_span = first.span();
    let group = match (first, iter.next()) {
        (TokenTree::Group(first), None) if first.delimiter() == Delimiter::Brace => {
            first.into_token_stream()
        }
        (first, second) => quote_spanned! { first_span =>  { #first #second #(#iter)* } },
    };

    quote_spanned! { first_span =>
        loop {
            #[allow(unreachable_code)]
            break {
                #[warn(unreachable_code)]
                #group
            };
        }
    }
}

const EST_EXPR_SIZE: usize = 20;

// TODO: Rename when less tired
fn call_on_block(
    tokens: &mut TokenStream,
    block: &syn::Block,
    f: impl FnOnce(&mut TokenStream, TokenStream),
) {
    match block.stmts.iter().rposition(|stmt| {
        matches!(
            stmt,
            syn::Stmt::Expr(_, _) | syn::Stmt::Macro(_) | syn::Stmt::Local(_),
        )
    }) {
        Some(i)
            if matches!(
                block.stmts[i],
                syn::Stmt::Expr(_, None)
                    | syn::Stmt::Macro(syn::StmtMacro {
                        semi_token: None,
                        ..
                    })
            ) && !can_block_break(block) =>
        {
            let (before, after) = block.stmts.split_at(i);
            let (stmt, after) = after.split_first().unwrap();

            let mut inner_tokens = TokenStream::new();
            inner_tokens.append_all(before);
            f(
                &mut inner_tokens,
                match stmt {
                    syn::Stmt::Expr(expr, None) => expr.to_token_stream(),
                    syn::Stmt::Macro(
                        mac @ syn::StmtMacro {
                            semi_token: None, ..
                        },
                    ) => mac.to_token_stream(),
                    _ => unreachable!(),
                },
            );
            inner_tokens.append_all(after);
            surround_with_block(tokens, block.brace_token.span.join(), inner_tokens, false);
        }
        _ => f(tokens, isolate_block(block)),
    }
}

fn call_on_maybe_block(
    tokens: &mut TokenStream,
    block: &parser::Block,
    f: impl FnOnce(&mut TokenStream, TokenStream),
) {
    match block {
        parser::Block::Valid(block) => call_on_block(tokens, block, f),
        parser::Block::Invalid { body, .. } => f(tokens, isolate_block(body)),
    }
}

fn surround_with_block(
    tokens: &mut TokenStream,
    span: Span,
    inner_tokens: TokenStream,
    require_brace: bool,
) {
    let mut inner_tokens = inner_tokens.into_iter().peekable();
    match inner_tokens.next() {
        Some(TokenTree::Group(first))
            if first.delimiter() == Delimiter::Brace && inner_tokens.peek().is_none() =>
        {
            let inner = first.stream();
            tokens.append_all(quote_spanned!(span => { #inner }));
        }
        Some(first) => tokens.append_all(quote_spanned!(span => {
            #first
            #(#inner_tokens)*
        })),
        None if require_brace => tokens.append_all(quote_spanned!(span => {})),
        None => {}
    }
}

enum BufEntry {
    Token(TokenTree),
    LitStr(syn::LitStr),
    Str(Cow<'static, str>),
}

impl BufEntry {
    fn lit_str(span: Span, value: &str) -> Self {
        Self::LitStr(syn::LitStr::new(value, span))
    }

    fn str(value: impl Into<Cow<'static, str>>) -> Self {
        Self::Str(value.into())
    }

    fn stringify(&self, span: Span) -> TokenStream {
        match self {
            BufEntry::Str(s) => quote_spanned! { span => #s },
            BufEntry::Token(token) => {
                quote_spanned! { span => stringify!(#token) }
            }
            BufEntry::LitStr(lit) => lit.to_token_stream(),
        }
    }
}

struct Buf(Vec<BufEntry>);

impl Buf {
    fn push_str(&mut self, s: impl Into<Cow<'static, str>>) {
        self.push(BufEntry::str(s));
    }
    fn push_lit_str(&mut self, span: Span, value: &str) {
        self.push(BufEntry::lit_str(span, value));
    }
    fn extend_tokens(&mut self, tokens: impl ToTokens) {
        for token in tokens.into_token_stream() {
            self.push(BufEntry::Token(token));
        }
    }

    fn name(&mut self, name: &parser::Name) {
        match name {
            parser::Name::Str(s) => self.push(BufEntry::LitStr(s.clone())),
            parser::Name::Parts(parts) => {
                for part in parts {
                    self.extend_tokens(part);
                }
            }
        }
    }
}

impl ops::Deref for Buf {
    type Target = Vec<BufEntry>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl ops::DerefMut for Buf {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

struct Generator<'a> {
    buf: Buf,
    sizes: Vec<usize>,
    space: bool,
    _phantom: PhantomData<&'a TemplBody>,
}

impl<'a> Generator<'a> {
    fn new() -> Self {
        Self {
            buf: Buf(vec![]),
            sizes: vec![],
            space: false,
            _phantom: PhantomData,
        }
    }

    fn write_escaped(&mut self, span: Span, value: impl fmt::Display) {
        pub struct EscapeWriter<'a, T>(&'a mut T);

        impl<T: Write> Write for EscapeWriter<'_, T> {
            #[inline]
            fn write_str(&mut self, s: &str) -> fmt::Result {
                askama_escape::Html.write_escaped(&mut *self.0, s)
            }
        }

        let mut s = String::new();
        write!(EscapeWriter(&mut s), "{value}").unwrap();
        self.buf.push_lit_str(span, &s);
    }

    fn top_size(&mut self) -> &mut usize {
        self.sizes.last_mut().unwrap()
    }

    #[inline]
    fn flush_buffer(&mut self, tokens: &mut TokenStream, span: Span) {
        *self.top_size() += self.buf.len();
        let buf = &mut self.buf;
        if !buf.is_empty() {
            let writer = writer();

            let entries = buf.drain(..).map(|e| e.stringify(span));

            tokens.append_all(quote_spanned! { span =>
                ::core::fmt::Write::write_str(#writer, concat!(#(#entries),*))?;
            });
        }
    }

    fn write_maybe_block(&mut self, tokens: &mut TokenStream, block: &parser::Block) {
        match block {
            parser::Block::Valid(block) => self.write_block(tokens, block),
            parser::Block::Invalid { brace, body } => {
                let block_span = brace.span.join();

                let crate_path = crate_path(block_span);
                let writer = writer();

                self.flush_buffer(tokens, brace.span.open());
                *self.top_size() += EST_EXPR_SIZE;

                let body = isolate_block(body);
                tokens.append_all(quote_spanned! { block_span =>
                    #crate_path::write_escaped(#writer, &#body)?;
                });
            }
        }
    }

    fn write_block(&mut self, tokens: &mut TokenStream, block: &syn::Block) {
        match try_stringify_block(block, true) {
            Some((span, s)) => {
                tokens.append_all(quote_spanned! { block.brace_token.span.open() => _ = });
                match can_block_break(block) {
                    true => tokens.append_all(isolate_block(block)),
                    false => {
                        block.brace_token.surround(tokens, |tokens| {
                            tokens.append_all(quote_spanned! { block.brace_token.span.open() =>
                                _ = 0;
                            });
                            tokens.append_all(&block.stmts);
                        });
                    }
                }
                tokens.append_all(quote_spanned! { block.brace_token.span.close() => ; });
                self.write_escaped(span, &s);
            }
            None => {
                self.flush_buffer(tokens, block.brace_token.span.open());
                call_on_block(tokens, block, |tokens, expr| {
                    let block_span = block.brace_token.span.join();
                    let crate_path = crate_path(block_span);
                    let writer = writer();

                    *self.top_size() += EST_EXPR_SIZE;
                    tokens.append_all(quote_spanned! { block_span =>
                        #crate_path::write_escaped(#writer, &(#expr))?;
                    });
                });
            }
        }
    }

    fn write_templ(&mut self, tokens: &mut TokenStream, span: Span, body: &'a TemplBody) {
        let crate_path = crate_path(Span::mixed_site());
        let context = context();
        let writer = writer();

        self.sizes.push(0);
        let mut inner_tokens = TokenStream::new();
        for node in &body.nodes {
            self.write_node(&mut inner_tokens, node);
        }
        self.flush_buffer(&mut inner_tokens, span);
        let size = self.sizes.pop().unwrap();

        let context_ty = body
            .use_context()
            .and_then(|u| u.colon_ty.as_ref())
            .map(|(colon, ty)| quote! { #colon #ty });
        let context_pat_owned;
        let context_pat: &dyn ToTokens = match body.use_context() {
            Some(u) => match &u.as_pat {
                Some((_, pat)) => pat,
                None => &u.context,
            },
            None => {
                context_pat_owned = quote_spanned! { span => _ };
                &context_pat_owned
            }
        };
        let children_owned;
        let children: &dyn ToTokens = match body.use_children() {
            Some(u) => match &u.as_pat {
                Some((_, pat)) => pat,
                None => &u.children,
            },
            None => {
                children_owned = quote_spanned! { span => _ };
                &children_owned
            }
        };
        tokens.append_all(quote_spanned! { span =>
            #crate_path::FnTemplate::new_sized(
                #size,
                move |#writer, #context @ #context_pat #context_ty, #children| {
                    #inner_tokens
                    #crate_path::Result::Ok(())
                },
            )
        })
    }

    fn write_match<T>(
        &mut self,
        tokens: &mut TokenStream,
        stmt: &'a parser::Match<T>,
        mut write_item: impl FnMut(&mut Self, &mut TokenStream, &'a T),
    ) {
        let parser::Match {
            pound,
            match_token,
            expr,
            brace,
            arms,
        } = stmt;

        self.flush_buffer(tokens, pound.span);

        let num_arms = arms.len();
        self.sizes.push(0);
        let init_space = self.space;

        let arms = arms.iter().map(
            |parser::match_stmt::Arm {
                 pat,
                 guard,
                 fat_arrow,
                 brace,
                 body,
             }| {
                let mut inner_tokens = TokenStream::new();
                self.space = init_space;
                for item in body {
                    write_item(self, &mut inner_tokens, item);
                }
                self.flush_buffer(&mut inner_tokens, brace.span.close());
                self.space = true;

                let guard = guard
                    .as_ref()
                    .map(|(if_token, cond)| quote! { #if_token #cond });

                let mut tokens = quote! { #pat #guard #fat_arrow };
                surround_with_block(&mut tokens, brace.span.join(), inner_tokens, true);
                tokens
            },
        );

        tokens.append_all(quote_spanned! { brace.span.join() =>
            #match_token #expr { #(#arms)* }
        });

        let avg_size = self.sizes.pop().unwrap() / num_arms;
        *self.top_size() += avg_size;
    }

    fn write_if<T>(
        &mut self,
        tokens: &mut TokenStream,
        stmt: &'a parser::If<T>,
        mut write_item: impl FnMut(&mut Self, &mut TokenStream, &'a T),
    ) {
        let parser::If {
            pound,
            if_token,
            cond,
            brace,
            body,
            else_if_branches,
            else_branch,
        } = stmt;

        self.flush_buffer(tokens, pound.span);

        let num_branches = 1 + else_if_branches.len() + else_branch.is_some() as usize;
        self.sizes.push(0);

        let init_space = self.space;
        let mut write_nodes = |tokens: &mut _, span, body| {
            self.space = init_space;
            for item in body {
                write_item(self, tokens, item);
            }
            self.flush_buffer(tokens, span);
            self.space = true;
        };

        let mut then_tokens = TokenStream::new();
        write_nodes(&mut then_tokens, brace.span.open(), body);

        tokens.append_all(quote! { #if_token #cond });
        surround_with_block(tokens, brace.span.join(), then_tokens, true);
        for branch in else_if_branches {
            let parser::if_stmt::ElseIfBranch {
                else_token,
                if_token,
                cond,
                brace,
                body,
            } = branch;
            let mut then_tokens = TokenStream::new();
            write_nodes(&mut then_tokens, brace.span.open(), body);

            tokens.append_all(quote! { #else_token #if_token #cond });
            surround_with_block(tokens, brace.span.join(), then_tokens, true);
        }
        if let Some(parser::if_stmt::ElseBranch {
            else_token,
            brace,
            body,
        }) = else_branch
        {
            let mut then_tokens = TokenStream::new();
            write_nodes(&mut then_tokens, brace.span.open(), body);

            tokens.append_all(quote! { #else_token });
            surround_with_block(tokens, brace.span.join(), then_tokens, true);
        }

        let avg_size = self.sizes.pop().unwrap() / num_branches;
        *self.top_size() += avg_size;
    }

    fn write_for<T>(
        &mut self,
        tokens: &mut TokenStream,
        stmt: &'a parser::For<T>,
        mut write_item: impl FnMut(&mut Self, &mut TokenStream, &'a T),
    ) {
        let parser::For {
            pound,
            for_token,
            pat,
            in_token,
            expr,
            brace,
            body,
        } = stmt;
        self.flush_buffer(tokens, pound.span);

        let mut inner_tokens = TokenStream::new();
        self.space = true;
        for item in body {
            write_item(self, &mut inner_tokens, item);
        }
        self.flush_buffer(&mut inner_tokens, brace.span.close());

        tokens.append_all(quote! { #for_token #pat #in_token #expr });
        surround_with_block(tokens, brace.span.join(), inner_tokens, true);
    }

    fn write_attr(&mut self, tokens: &mut TokenStream, attr: &'a parser::Attr) {
        match attr {
            parser::Attr::Html(parser::attrs::HtmlAttr { name, value }) => match value {
                parser::attrs::HtmlAttrValue::None => {
                    self.buf.push_str(" ");
                    self.buf.name(name);
                }
                parser::attrs::HtmlAttrValue::Ident(eq, value) => {
                    self.buf.push_str(" ");
                    self.buf.name(name);
                    self.buf.extend_tokens(quote! { #eq #value });
                }
                parser::attrs::HtmlAttrValue::Str(eq, value) => {
                    self.buf.push_str(" ");
                    self.buf.name(name);
                    self.buf.extend_tokens(eq);
                    self.buf.push_str("\"");
                    self.write_escaped(value.span(), value.value());
                    self.buf.push_str("\"");
                }
                parser::attrs::HtmlAttrValue::Int(eq, value) => {
                    self.buf.push_str(" ");
                    self.buf.name(name);
                    self.buf.extend_tokens(eq);
                    self.buf.push_str("\"");
                    self.write_escaped(value.span(), value);
                    self.buf.push_str("\"");
                }
                parser::attrs::HtmlAttrValue::Float(eq, value) => {
                    self.buf.push_str(" ");
                    self.buf.name(name);
                    self.buf.extend_tokens(eq);
                    self.buf.push_str("\"");
                    self.write_escaped(value.span(), value);
                    self.buf.push_str("\"");
                }
                parser::attrs::HtmlAttrValue::Block(toggle, eq, cond) => match toggle {
                    Some(toggle) => {
                        self.flush_buffer(tokens, toggle.span);

                        call_on_maybe_block(tokens, cond, |tokens, expr| {
                            let crate_path = crate_path(toggle.span);
                            let writer = writer();

                            let mut inner_tokens = TokenStream::new();

                            self.buf.push_str(" ");
                            self.buf.name(name);
                            self.flush_buffer(&mut inner_tokens, cond.brace_span().open());

                            *self.top_size() += EST_EXPR_SIZE;
                            tokens.append_all(quote_spanned! { toggle.span =>
                                _ = 0;
                                if let Some(view) = #crate_path::OptAttrValue::to_opt_attr_value(#expr) {
                                    #inner_tokens
                                    #crate_path::write_escaped(#writer, &view)?;
                                }
                            });
                        });
                    }
                    None => {
                        self.buf.push_str(" ");
                        self.buf.name(name);
                        self.buf.extend_tokens(eq);
                        self.buf.push_str("\"");
                        self.write_maybe_block(tokens, cond);
                        self.buf.push_str("\"");
                    }
                },
            },
            parser::Attr::If(stmt) => self.write_if(tokens, stmt, Self::write_attr),
            parser::Attr::Match(stmt) => self.write_match(tokens, stmt, Self::write_attr),
            parser::Attr::Scope(parser::Scope { brace, body, .. }) => {
                brace.surround(tokens, |tokens| {
                    for attr in body {
                        self.write_attr(tokens, attr);
                    }
                })
            }
            parser::Attr::Let(stmt) => {
                stmt.let_token.to_tokens(tokens);
                stmt.pat.to_tokens(tokens);
                stmt.init.to_tokens(tokens);
                stmt.semi.to_tokens(tokens);
            }
            parser::Attr::Spread(block) => call_on_maybe_block(tokens, block, |tokens, expr| {
                self.flush_buffer(tokens, block.brace_span().open());

                let block_span = block.brace_span().join();
                let crate_path = crate_path(block_span);
                let writer = writer();

                *self.top_size() += EST_EXPR_SIZE;
                tokens.append_all(quote_spanned! { block_span =>
                    #crate_path::Attributes::render_into(#expr, #writer)?;
                });
            }),
        }
    }

    fn write_element(&mut self, tokens: &mut TokenStream, element: &'a Element) {
        if self.space {
            self.buf.push_str(" ");
        }
        self.space = false;

        let parser::element::OpenTag {
            lt,
            name,
            attrs,
            slash,
            gt,
        } = &element.open;

        self.buf.extend_tokens(lt);
        self.buf.name(name);

        let mut inner_tokens = TokenStream::new();
        for attr in attrs {
            self.write_attr(&mut inner_tokens, attr);
        }
        surround_with_block(tokens, lt.span, inner_tokens, false);
        self.buf.extend_tokens(gt);

        let mut inner_tokens = TokenStream::new();
        for node in &element.nodes {
            self.write_node(&mut inner_tokens, node);
        }
        surround_with_block(tokens, gt.span, inner_tokens, false);

        if let Some(slash) = slash {
            if !SELF_CLOSING.contains(&&*name.to_string()) {
                self.buf.extend_tokens(quote! { #lt #slash });
                self.buf.name(name);
                self.buf.extend_tokens(gt);
            }
        } else if let Some(parser::element::CloseTag {
            lt,
            slash,
            name,
            gt,
        }) = &element.close
        {
            self.buf.extend_tokens(quote! { #lt #slash });
            self.buf.name(name);
            self.buf.extend_tokens(gt);
        }
        self.space = true;
    }

    fn write_call(&mut self, tokens: &mut TokenStream, call: &'a parser::Call) {
        let parser::Call { pound, expr, end } = call;

        self.flush_buffer(tokens, pound.span);

        let expr = match can_expr_break(expr) {
            true => isolate_block(expr),
            false => expr.to_token_stream(),
        };

        let crate_path = crate_path(pound.span);
        let context = context();
        let writer = writer();

        match end {
            parser::call::End::Semi(semi) => {
                tokens.append_all(quote_spanned! { pound.span =>
                    #crate_path::Template::render_into(
                        &(#expr),
                        #writer,
                        #context
                    )? #semi
                });
            }
            parser::call::End::Children(brace, body) => {
                let mut inner_tokens = TokenStream::new();

                self.write_templ(&mut inner_tokens, brace.span.join(), body);

                tokens.append_all(quote_spanned! { pound.span =>
                    #crate_path::Template::render_with_children_into(
                        &(#expr),
                        #writer,
                        #context,
                        &#inner_tokens,
                    )?;
                });
            }
        }
    }

    fn write_node(&mut self, tokens: &mut TokenStream, node: &'a Node) {
        match node {
            Node::Entity(entity) => self.buf.extend_tokens(entity),
            Node::Doctype(parser::Doctype {
                lt,
                bang,
                doctype,
                name,
                gt,
            }) => {
                self.buf.extend_tokens(quote! { #lt #bang #doctype });
                self.buf.push_str(" ");
                self.buf.name(name);
                self.buf.extend_tokens(gt);
            }
            Node::Element(element) => self.write_element(tokens, element),
            Node::RawText(text) => {
                for token in text.tokens.clone() {
                    match token {
                        TokenTree::Punct(punct)
                            if matches!(
                                punct.as_char(),
                                '.' | ',' | ':' | ';' | '!' | '?' | '%'
                            ) =>
                        {
                            self.space = true;
                            self.buf.extend_tokens(punct)
                        }
                        TokenTree::Punct(punct)
                            if matches!(punct.as_char(), '$' | '#' | '¿' | '¡') =>
                        {
                            if self.space {
                                self.space = false;
                                self.buf.push_str(" ");
                            }
                            self.buf.extend_tokens(punct)
                        }
                        _ => {
                            if self.space {
                                self.buf.push_str(" ");
                            }
                            self.space = true;
                            self.write_escaped(token.span(), token);
                        }
                    }
                }
            }
            Node::Paren(paren, nodes) => {
                if self.space {
                    self.buf.push_str(" ");
                }
                self.buf.push_lit_str(paren.span.open(), "(");
                self.space = false;
                for node in nodes {
                    self.write_node(tokens, node);
                }
                self.buf.push_lit_str(paren.span.close(), ")");
            }
            Node::Bracket(bracket, nodes) => {
                if self.space {
                    self.buf.push_str(" ");
                }
                self.buf.push_lit_str(bracket.span.open(), "[");
                self.space = false;
                for node in nodes {
                    self.write_node(tokens, node);
                }
                self.buf.push_lit_str(bracket.span.close(), "]");
            }
            Node::Expr(block) => {
                if self.space {
                    self.buf.push_str(" ");
                }
                self.space = true;
                self.write_maybe_block(tokens, block);
            }
            Node::If(stmt) => self.write_if(tokens, stmt, Self::write_node),
            Node::Match(stmt) => self.write_match(tokens, stmt, Self::write_node),
            Node::For(stmt) => self.write_for(tokens, stmt, Self::write_node),
            Node::With(parser::With {
                with,
                ty,
                eq,
                expr,
                brace,
                nodes,
                ..
            }) => {
                brace.surround(tokens, |tokens| {
                    let context = context();
                    let ty = ty.as_ref().map(|(col, ty)| quote! { #col #ty });
                    tokens.append_all(quote_spanned! { with.span =>
                        let #context #ty #eq #expr;
                    });
                    for node in nodes {
                        self.write_node(tokens, node);
                    }
                });
            }
            Node::Scope(parser::Scope { brace, body, .. }) => brace.surround(tokens, |tokens| {
                for node in body {
                    self.write_node(tokens, node);
                }
            }),
            Node::Let(stmt) => {
                stmt.let_token.to_tokens(tokens);
                stmt.pat.to_tokens(tokens);
                stmt.init.to_tokens(tokens);
                stmt.semi.to_tokens(tokens);
            }
            Node::Call(call) => self.write_call(tokens, call),
        }
    }
}

/// The [`templ!`] macro compiles down your template to an `FnTemplate` closure.
/// It uses HTML like syntax to write templates, except:
///
/// 1. All tags must close with a slash (e.g. `<img src="..." />`).
///    The slash will be removed for self closing tags during compilation,
///    or expanded into a closing tag for all other elements (e.g. `<script src="..." />`)
/// 2. Element and attribute names aren't as flexible as HTML,
///    to use names that aren't supported or to prevent "name merging" you can use strings.
///    (e.g. `<"input" "type"="hidden" "value"="..." />`)
/// 3. Unquoted attribute names cannot end with a `?`.
/// 4. All parenthesis and brackets must have a matching one inside the current element.
/// 6. Raw text cannot contain certain characters (`#`, `<`, `&`, etc).
///
/// ```rust
/// # use templr::{templ, Template};
/// let t = templ! {
///     <div class="hello world" hidden "@click"="doSomething()">
///         Lorem ipsum dolor sit amet.
///     </div>
/// };
/// let html = t.render(&()).unwrap();
/// assert_eq!(
///     html,
///     r#"<div class="hello world" hidden @click="doSomething()">Lorem ipsum dolor sit amet.</div>"#,
/// );
/// ```
///
/// # Interpolation
/// You can interpolate values using curly braces `{...}`. These can be used inside text.
/// Interpolating string literals is done at compile time, so feel free to do that when raw text
/// isn't sufficient: (e.g. `{"#1 winner"}`).
///
/// You may also use the question mark operator (`?`) inside the templates as long as they're
/// compatible with [`anyhow`].
///
/// ```rust
/// # use templr::{templ, Template};
/// let name = "Zecver";
/// let t = templ! {
///     Hello, {name}!
/// };
/// let html = t.render(&()).unwrap();
/// assert_eq!(html, format!("Hello, {name}!"));
/// ```
///
/// # Attributes
/// Attributes may have a constant value like normal html, (e.g. `attr="..."`).
/// Boolean attributes are likewise supported (e.g. `disabled`).
/// You can event have unquoted identifiers and number literals as values (e.g. `value=true`).
///
/// Other than static attributes you can assign interpolated values using `attr={...}`.
/// This will quote and escape the given value, so no worries.
/// You can use optional attributes (e.g. `attr?={true}`).
///
/// Finally, you can omit the attribute name and use the `Attributes` trait instead.
///
/// ```rust
/// # use templr::{templ, Template};
/// let name = Some("Zecver");
/// let value = "false";
/// let t = templ! {
///     <span
///         hidden
///         hello="world"
///         name?={name}
///         value={value}
///         {..[("hey", "no")]}
///     >
///         Hello
///     </span>
/// };
/// let html = t.render(&()).unwrap();
/// assert_eq!(
///     html,
///     r#"<span hidden hello="world" name="Zecver" value="false" hey="no">Hello</span>"#,
/// );
/// ```
/// # Statements
/// templr supports `#if`, `#match`, `#for`, `#let`, and `#{}`. These can be nodes or attributes
/// (except `#for`).
/// These statements function like their rust counterparts but they must start with a `#`.
///
/// ```rust
/// # use templr::{templ, Template};
/// let hide_list = false;
/// let list_type = None::<&str>;
/// let n = 2;
/// let error = Some("oops");
/// let t = templ! {
///     #match error {
///         Some(error) => {
///             <div class="err">{error}</div>
///         }
///         None => {
///             {"All's well."}
///         }
///     }
///     <ul
///         #if hide_list {
///             style="list-style: none; margin-left: 0;"
///         } else if let Some(list_type) = list_type {
///             style={format_args!("list-style: {list_type};")}
///         }
///     >
///         #{
///             #let n = n + 1;
///             #let range = 1..=n;
///             #for i in range {
///                 <li>No {i}</li>
///             }
///         }
///     </ul>
///     {n}
/// };
/// let html = t.render(&()).unwrap();
/// assert_eq!(
///     html,
///     r#"<div class="err">oops</div> <ul> <li>No 1</li> <li>No 2</li> <li>No 3</li></ul> 2"#
/// );
/// ```
///
/// # Template Composition
/// You can use template composition by putting a `#`, then a template expression,
/// and finally a closing semicolon or pass children with curly braces.
///
/// A template can catch children passed to it using the `#use children as pattern;` command at the top.
/// You can omit the `as pattern` part and the pattern will be `children`.
///
/// ```rust
/// # use templr::{templ, Template};
/// let hello = templ! {
///     #use children;
///
///     {"Hello, "}
///     #children;
///     !
/// };
///
/// let t = templ! {
///     #hello {
///         world
///     }
/// };
/// let html = t.render(&()).unwrap();
/// assert_eq!(html, "Hello, world!");
/// ```
///
/// # Context
/// Templates pass a context implicitly when instantiating. This is useful to avoid constantly
/// passing around arguments. The context defaults to `()`.
///
/// You can use it using `#use context as pattern: type`. When `as pattern` is omitted, `context`
/// is the pattern. While when `: type` is omitted, the type is infered.
///
/// To use templates with a different context type, you can use the `with` statement to change the
/// context for a specific block. You may supply an optional type after the `context` keyword
/// (e.g. `with context [: type] = value { ... }`).
///
/// ```rust
/// # use templr::{templ, Template};
/// let hello = templ! {
///     #use children;
///
///     {"Hello, "}
///     #children;
///     !
/// };
///
/// let t = templ! {
///     #hello {
///         #use context as name;
///         {name}
///     }
/// };
/// assert_eq!(t.render("Zaknar").unwrap(), r"Hello, Zaknar!");
///
/// let name = "Mannier";
/// let t2 = templ! {
///     #with context = name {
///         #t;
///     }
/// };
/// assert_eq!(t2.render(&()).unwrap(), r"Hello, Mannier!");
/// ```
///
/// [`anyhow`]: https://docs.rs/anyhow/
#[proc_macro]
pub fn templ(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let body = syn::parse_macro_input!(tokens as TemplBody);

    let mut tokens = TokenStream::new();
    let mut generator = Generator::new();
    generator.write_templ(&mut tokens, Span::call_site(), &body);
    tokens.into()
}

struct AttrsBody(Vec<parser::Attr>);

impl syn::parse::Parse for AttrsBody {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut attrs = vec![];
        while !input.is_empty() {
            attrs.push(input.parse()?);
        }
        Ok(Self(attrs))
    }
}

/// This renders the attributes into a `PrerenderedAttrs` struct. This can later be used for
/// attributes instantiation.
///
/// ```rust
/// # use templr::{templ, Template, attrs};
///
/// let attrs = &attrs! { style="color: red;" class="hello world" }.unwrap();
///
/// let t = templ! {
///     <ul>
///         <li {attrs}>Item 1</li>
///         <li {attrs}>Item 2</li>
///         <li {attrs}>Item 3</li>
///         <li {attrs}>Item 4</li>
///     </ul>
/// };
/// assert_eq!(
///     t.render(&()).unwrap(),
///     r#"<ul><li style="color: red;" class="hello world">Item 1</li> <li style="color: red;" class="hello world">Item 2</li> <li style="color: red;" class="hello world">Item 3</li> <li style="color: red;" class="hello world">Item 4</li></ul>"#,
/// );
/// ```
#[proc_macro]
pub fn attrs(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let body = syn::parse_macro_input!(tokens as AttrsBody);

    let mut inner_tokens = TokenStream::new();
    let mut generator = Generator::new();
    generator.sizes.push(0);
    for attr in &body.0 {
        generator.write_attr(&mut inner_tokens, attr);
    }
    generator.flush_buffer(&mut inner_tokens, Span::call_site());

    let writer = writer();
    let crate_path = crate_path(Span::call_site());
    From::from(quote! {
        (move || -> #crate_path::Result<_> {
            let mut #writer = ::std::string::String::new();
            {
                let #writer = &mut #writer;
                #inner_tokens
            }
            #crate_path::Result::Ok(
                #crate_path::attrs::PrerenderedAttrs::from_raw_unchecked(#writer),
            )
        })()
    })
}

/// This derives the `Template` trait using all `ToTemplate` implementations.
/// This implementation runs `ToTemplate::to_template` for every method of the `Template` trait,
/// so that method should be very lightweight (like using [`templ!`]).
///
/// ```rust
/// # use templr::{templ, ToTemplate, Template};
/// #[derive(Template)]
/// struct Greet<'a> {
///     name: &'a str,
/// }
///
/// impl ToTemplate for Greet<'_> {
///     fn to_template(&self) -> impl Template + '_ {
///         templ! {
///             Hello, {self.name}!
///         }
///     }
/// }
///
/// let t = templ! {
///     #(Greet { name: "baba" });
/// };
/// let html = t.render(&()).unwrap();
/// assert_eq!(html, "Hello, baba!");
/// ```
#[proc_macro_derive(Template)]
pub fn derive_template(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(tokens as syn::DeriveInput);

    let name = input.ident;

    let crate_path = crate_path(Span::call_site());

    let ctx_ty = Ident::new("__TemplrCtx", Span::mixed_site());
    let (_, ty_generics, _) = input.generics.split_for_impl();

    let mut modified_generics = input.generics.clone();
    modified_generics.params.push(parse_quote! {
        #ctx_ty: ?Sized
    });
    modified_generics
        .make_where_clause()
        .predicates
        .push(parse_quote! {
            Self: #crate_path::ToTemplate<#ctx_ty>
        });

    let (impl_generics, _, where_clause) = modified_generics.split_for_impl();

    From::from(quote! {
        impl #impl_generics #crate_path::Template<#ctx_ty> for #name #ty_generics
        #where_clause
        {
            fn size_hint(&self) -> usize {
                #crate_path::ToTemplate::<#ctx_ty>::to_template(self)
                    .size_hint()
            }
            fn render_with_children_into(
                &self,
                writer: &mut dyn ::std::fmt::Write,
                ctx: &#ctx_ty,
                children: &dyn #crate_path::Template<#ctx_ty>,
            ) -> #crate_path::Result<()> {
                #crate_path::ToTemplate::<#ctx_ty>::to_template(self)
                    .render_with_children_into(writer, ctx, children)
            }
            fn render_into(&self, writer: &mut dyn ::std::fmt::Write, ctx: &#ctx_ty) -> #crate_path::Result<()> {
                #crate_path::ToTemplate::<#ctx_ty>::to_template(self)
                    .render_into(writer, ctx)
            }
            fn write_into(&self, writer: &mut dyn ::std::io::Write, ctx: &#ctx_ty) -> ::std::io::Result<()> {
                #crate_path::ToTemplate::<#ctx_ty>::to_template(self)
                    .write_into(writer, ctx)
            }
            fn render(&self, ctx: &#ctx_ty) -> #crate_path::Result<String> {
                #crate_path::ToTemplate::<#ctx_ty>::to_template(self)
                    .render(ctx)
            }
        }
    })
}
