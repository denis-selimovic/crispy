# crispy

Lisp interpreter written in Rust. It supports subset of Scheme dialect for Rust. Easily extendable to support full Scheme dialect.

## CLI

You can start LISP repl with following command.

> `$ cargo run --release --bin repl`

You can parse lisp file with following command.

> `$ cargo run --release --bin cli -- <path_to_lisp_file>`

## Example usage

<p align="center" width="100%">
    <img width="70%" src="example/lisp.gif"> 
</p>

## Tests

To run tests use:

> `$ cargo test`
