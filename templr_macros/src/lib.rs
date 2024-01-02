use std::{
    fmt::{self, Write},
    marker::PhantomData,
};

use askama_escape::Escaper;
use parser::{Element, Name};
use proc_macro2::{Delimiter, Span, TokenStream, TokenTree};
use quote::{quote, quote_spanned, ToTokens, TokenStreamExt};
use syn::{punctuated::Punctuated, spanned::Spanned, Ident, Token};
use templr_parser::{self as parser, Node, TemplBody};

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
            return true;
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
        syn::Expr::Continue(expr) => can_attrs_break(&expr.attrs),
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

fn try_stringify_expr(expr: &syn::Expr, can_break: bool) -> Option<String> {
    if can_break && can_expr_break(expr) {
        return None;
    }
    match expr {
        syn::Expr::Lit(syn::ExprLit { attrs, lit }) if attrs.is_empty() => match lit {
            syn::Lit::Str(s) => Some(s.value()),
            syn::Lit::Byte(byte) => Some(byte.value().to_string()),
            syn::Lit::Char(ch) => Some(ch.value().to_string()),
            syn::Lit::Int(int) => Some(int.to_string()),
            syn::Lit::Float(float) => Some(float.to_string()),
            syn::Lit::Bool(bool) => Some(bool.value().to_string()),
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

fn try_stringify_block(block: &syn::Block, can_break: bool) -> Option<String> {
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
            for stmt in before {
                stmt.to_tokens(&mut inner_tokens);
            }
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
            for stmt in after {
                stmt.to_tokens(&mut inner_tokens);
            }
            tokens.append_all(quote::quote_spanned! { block.brace_token.span.span() => {
                #inner_tokens
            } });
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

struct Generator<'a> {
    buf: String,
    sizes: Vec<usize>,
    _phantom: PhantomData<&'a TemplBody>,
}

impl<'a> Generator<'a> {
    fn new() -> Self {
        Self {
            buf: String::new(),
            sizes: vec![],
            _phantom: PhantomData,
        }
    }

    fn write_escaped(&mut self, value: impl fmt::Display, space: bool) {
        pub struct EscapeWriter<'a>(&'a mut String);

        impl Write for EscapeWriter<'_> {
            #[inline]
            fn write_str(&mut self, s: &str) -> std::fmt::Result {
                askama_escape::Html.write_escaped(&mut *self.0, s)
            }
        }

        if space && !self.buf.ends_with(|ch: char| ch.is_ascii_whitespace()) {
            self.buf.push(' ');
        }
        write!(EscapeWriter(&mut self.buf), "{value}").unwrap();
        if space && !self.buf.ends_with(|ch: char| ch.is_ascii_whitespace()) {
            self.buf.push(' ');
        }
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

            tokens.append_all(quote_spanned! { span =>
                ::core::fmt::Write::write_str(#writer, #buf)?;
            });
            buf.clear();
        }
    }

    fn write_maybe_block(&mut self, tokens: &mut TokenStream, block: &parser::Block) {
        match block {
            parser::Block::Valid(block) => self.write_block(tokens, block),
            parser::Block::Invalid { brace, body } => {
                let block_span = brace.span.span();

                let crate_path = crate_path(block_span);
                let writer = writer();

                if !self.buf.ends_with(|ch: char| ch.is_ascii_whitespace()) {
                    self.buf.push(' ');
                }
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
            Some(s) => {
                match can_block_break(block) {
                    true => tokens.append_all(isolate_block(block)),
                    false => block.to_tokens(tokens),
                }
                tokens.append_all(quote_spanned! { block.brace_token.span.close() => ; });
                self.write_escaped(&s, true);
            }
            None => {
                if !self.buf.ends_with(|ch: char| ch.is_ascii_whitespace()) {
                    self.buf.push(' ');
                }
                self.flush_buffer(tokens, block.brace_token.span.open());
                call_on_block(tokens, block, |tokens, expr| {
                    let block_span = block.brace_token.span.span();
                    let crate_path = crate_path(block_span);
                    let writer = writer();

                    *self.top_size() += EST_EXPR_SIZE;
                    tokens.append_all(quote_spanned! { block_span => {
                        #crate_path::write_escaped(#writer, &(#expr))?;
                    } });
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
            .use_context
            .as_ref()
            .and_then(|u| u.colon_ty.as_ref())
            .map(|(colon, amp, ty)| quote! { #colon #amp #ty });
        let context_pat_owned;
        let context_pat: &dyn ToTokens = match &body.use_context {
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
        let children: &dyn ToTokens = match &body.use_children {
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
                move |#writer, #context @ #context_pat #context_ty, #children| {
                    #inner_tokens
                    #crate_path::Result::Ok(())
                },
                #size,
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

        let arms = arms.iter().map(
            |parser::match_stmt::MatchArm {
                 pat,
                 guard,
                 fat_arrow,
                 brace,
                 body,
             }| {
                let mut inner_tokens = TokenStream::new();
                for item in body {
                    write_item(self, &mut inner_tokens, item);
                }
                self.flush_buffer(&mut inner_tokens, brace.span.close());

                let guard = guard
                    .as_ref()
                    .map(|(if_token, cond)| quote! { #if_token #cond });

                quote_spanned! { brace.span.span() =>
                    #pat #guard #fat_arrow { #inner_tokens }
                }
            },
        );

        tokens.append_all(quote_spanned! { brace.span.span() =>
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
        let mut write_nodes = |tokens: &mut _, span, body| {
            for item in body {
                write_item(self, tokens, item);
                // self.write_node(tokens, item);
            }
            self.flush_buffer(tokens, span);
        };

        let mut then_tokens = TokenStream::new();
        write_nodes(&mut then_tokens, brace.span.open(), body);

        let else_branch = else_branch.as_ref().map(
            |parser::if_stmt::ElseBranch {
                 else_token,
                 brace,
                 body,
             }| {
                let mut then_tokens = TokenStream::new();
                write_nodes(&mut then_tokens, brace.span.open(), body);

                quote_spanned! { brace.span.span() => #else_token #if_token #cond { #then_tokens } }
            },
        );

        let else_if_branches = else_if_branches.iter().map(
            |parser::if_stmt::ElseIfBranch {
                 else_token,
                 if_token,
                 cond,
                 brace,
                 body,
             }| {
                let mut then_tokens = TokenStream::new();
                write_nodes(&mut then_tokens, brace.span.open(), body);

                quote_spanned! { brace.span.span() => #else_token #if_token #cond { #then_tokens } }
            },
        );

        tokens.append_all(quote_spanned! { brace.span.span() =>
            #if_token #cond { #then_tokens } #(#else_if_branches)* #else_branch
        });

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
        for item in body {
            write_item(self, &mut inner_tokens, item);
        }
        self.flush_buffer(&mut inner_tokens, brace.span.close());

        tokens.append_all(quote_spanned! { brace.span.span() =>
            #for_token #pat #in_token #expr {
                #inner_tokens
            }
        })
    }

    fn write_attr(&mut self, tokens: &mut TokenStream, attr: &'a parser::Attr) {
        match attr {
            parser::Attr::Html(parser::attrs::HtmlAttr { name, value }) => match value {
                parser::attrs::HtmlAttrValue::None => {
                    write!(self.buf, " {name}").unwrap();
                }
                parser::attrs::HtmlAttrValue::Ident(_, value) => {
                    write!(self.buf, " {name}={value}").unwrap();
                }
                parser::attrs::HtmlAttrValue::Str(_, value) => {
                    write!(self.buf, " {name}=\"",).unwrap();
                    self.write_escaped(value.value(), false);
                    write!(self.buf, "\"").unwrap();
                }
                parser::attrs::HtmlAttrValue::Int(_, value) => {
                    write!(self.buf, " {name}=\"",).unwrap();
                    self.write_escaped(&value, false);
                    write!(self.buf, "\"").unwrap();
                }
                parser::attrs::HtmlAttrValue::Float(_, value) => {
                    write!(self.buf, " {name}=\"",).unwrap();
                    self.write_escaped(&value, false);
                    write!(self.buf, "\"").unwrap();
                }
                parser::attrs::HtmlAttrValue::Block(toggle, _, cond) => match toggle {
                    Some(toggle) => {
                        self.flush_buffer(tokens, toggle.span);

                        call_on_maybe_block(tokens, cond, |tokens, expr| {
                            let crate_path = crate_path(toggle.span);
                            let writer = writer();

                            let mut inner_tokens = TokenStream::new();

                            write!(self.buf, " {name}").unwrap();
                            self.flush_buffer(&mut inner_tokens, cond.brace_span().open());

                            *self.top_size() += EST_EXPR_SIZE;
                            tokens.append_all(quote_spanned! { toggle.span =>
                                _ = ();
                                if let Some(view) = #crate_path::OptAttrValue::to_opt_attr_value(#expr) {
                                    #inner_tokens
                                    #crate_path::write_escaped(#writer, &view)?;
                                }
                            });
                        });
                    }
                    None => {
                        write!(self.buf, " {name}=\"").unwrap();
                        self.write_maybe_block(tokens, cond);
                        write!(self.buf, "\"").unwrap();
                    }
                },
            },
            parser::Attr::If(stmt) => self.write_if(tokens, stmt, Self::write_attr),
            parser::Attr::Match(stmt) => self.write_match(tokens, stmt, Self::write_attr),
            parser::Attr::Block(parser::PoundBlock { brace, body, .. }) => {
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

                let block_span = block.brace_span().span();
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
        const SELF_CLOSING: &[&str] = &[
            "arena", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "param",
            "source", "track", "wbr", // html
        ];

        let parser::element::OpenTag {
            lt,
            name,
            attrs,
            slash,
            gt,
        } = &element.open;

        let name = name.to_string();
        write!(self.buf, "<{name}").unwrap();

        let mut inner_tokens = TokenStream::new();
        for attr in attrs {
            self.write_attr(&mut inner_tokens, attr);
        }
        tokens.append_all(quote_spanned!(lt.span => {
            #inner_tokens
        }));
        write!(self.buf, ">").unwrap();

        let mut inner_tokens = TokenStream::new();
        for node in &element.nodes {
            self.write_node(&mut inner_tokens, node);
        }
        tokens.append_all(quote_spanned!(gt.span => {
            #inner_tokens
        }));

        if slash.is_some() {
            if !SELF_CLOSING.contains(&&*name) {
                write!(self.buf, "</{name}>").unwrap();
            }
            return;
        }

        write!(self.buf, "</{name}>").unwrap();
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
                    #crate_path::Template::render_into(&(#expr), #writer, #context)? #semi
                });
            }
            parser::call::End::Children(brace, body) => {
                let mut inner_tokens = TokenStream::new();

                self.write_templ(&mut inner_tokens, brace.span.span(), body);

                tokens.append_all(quote_spanned! { pound.span =>
                    #crate_path::Template::render_with_children_into(&(#expr), #writer, #context, &#inner_tokens)?;
                });
            }
        }
    }

    fn write_node(&mut self, tokens: &mut TokenStream, node: &'a Node) {
        match node {
            Node::Entity(entity) => write!(self.buf, "{entity}").unwrap(),
            Node::Doctype(doctype) => write!(self.buf, "{doctype}").unwrap(),
            Node::Element(element) => self.write_element(tokens, element),
            Node::RawText(text) => self.write_escaped(text, true),
            Node::Expr(block) => self.write_maybe_block(tokens, block),
            Node::If(stmt) => self.write_if(tokens, stmt, Self::write_node),
            Node::Match(stmt) => self.write_match(tokens, stmt, Self::write_node),
            Node::For(stmt) => self.write_for(tokens, stmt, Self::write_node),
            Node::Block(parser::PoundBlock { brace, body, .. }) => {
                brace.surround(tokens, |tokens| {
                    for node in body {
                        self.write_node(tokens, node);
                    }
                })
            }
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

#[proc_macro]
pub fn templ(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let body = syn::parse_macro_input!(tokens as TemplBody);

    let mut tokens = TokenStream::new();
    let mut generator = Generator::new();
    generator.write_templ(&mut tokens, Span::call_site(), &body);
    tokens.into()
}
