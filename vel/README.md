# Vel's Language Engine

<p><img src="../logo/logo.svg" height="70" alt="Vel's logo" /></p>

The heart of Vel, its language engine, written in Rust.

## Features

- [x] Lexer ([`lex`](./src/lex.rs))
- [ ] Formatter
- [ ] Parser
- [ ] Type checker
- [ ] LSP server

## Install

This crate is currently not published to [crates.io](https://crates.io/).

To build and install the latest binary of `vel` from GitHub, run:
```shell
> cargo install --git https://github.com/hopv/vel
```

You can also build and install your local version of `vel` by running:
```shell
> cargo install --path path/to/vel
```
