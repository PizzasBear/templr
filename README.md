# templr

A [templ](https://github.com/a-h/templ) inspired rust template engine.
templr generates Rust code from your templates at compile time using a macro.

## Features

- Template instantiation (with children!)
- Context for passing info deep
- Rust-like `for`, `if-else`, `if-let`, `match`, and `let` statements (allowed in start tags!)
- Full rust variables
- Opt-out HTML escaping
- Nice attributes
- Planned support for common web frameworks.

## How to get started

First, add the russx dependancy to your crate's `Cargo.toml`:

```sh
# Adds the git version, change this to the crates.io one when available
cargo add --git https://github.com/PizzasBear/templr templr
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
