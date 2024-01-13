# templr

[![Documentation](https://docs.rs/templr/badge.svg)](https://docs.rs/templr/)
[![Latest version](https://img.shields.io/crates/v/templr.svg)](https://crates.io/crates/templr)

A [templ](https://github.com/a-h/templ) inspired rust template engine.
templr generates Rust code from your templates at compile time using a macro.

## Features

- Template instantiation (with children!)
- Context for passing info deep
- Rust-like `for`, `if-else`, `if-let`, `match`, and `let` statements (allowed in start tags!)
- Full rust variables
- Opt-out HTML escaping
- Nice attributes
- We have a formatter: [templrfmt](https://github.com/PizzasBear/templrfmt)!
- Optional built-in support for Actix-Web, Axum, Gotham, Rocket, Salvo, Tide, and Warp web frameworks.

## How to get started

First, add the russx dependancy to your crate's `Cargo.toml`:

```sh
cargo add templr
```

In any Rust file inside your crate, add the following:

```rust
use templr::{Template, templ};

pub fn hello(name: &str) -> impl Template + '_ {
    templ! {
        <p>Hello, {name}!</p>
    }
}

fn main() {
    let html = hello("world").render(&()).unwrap();
    println!("{html}");
}
```

You should be able to compile and run this code.
