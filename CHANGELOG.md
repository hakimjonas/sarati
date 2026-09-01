# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.0.0-alpha] - 2026-09

Initial public release.

### Added

- `SaratiCodec`: offset-based binary encoding and decoding for primitives, collections, and case classes and enums derived at compile time.
- Format ASTs for JSON, TOML, YAML, and XML, decoupled from text parsing, with a formatter for each.
- `Decoder`/`Encoder` typeclasses that map the format ASTs to and from typed Scala data, with derivation for case classes.
- An XPath 1.0 evaluator over the XML AST: all 13 axes, predicates with proximity positions, the full operator ladder, and the 27-function core library.
