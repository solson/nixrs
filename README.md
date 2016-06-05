# Nixrs

A [Nix][nix] language interpreter written in [Rust][rust].

## Progress

As of June 4th, 2016, the [lexer](src/parse.rs) is partially implemented and
essentially nothing else is done.

## Run a test

```sh
cargo run <file to lex>
```

## License

Licensed under the MIT license. See [LICENSE](LICENSE) or
http://opensource.org/licenses/MIT.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you shall be licensed as above, without any
additional terms or conditions.

[nix]: http://nixos.org/nix/
[rust]: https://www.rust-lang.org/
