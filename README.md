# alumbra.analyzer

This library contains means of transforming raw GraphQL ASTs (as produced by
e.g. [alumbra.parser][alumbra-parser]) into formats more suitable for validation
or execution. It is based on the specs defined in [alumbra.spec][alumbra-spec].

[![Build Status](https://travis-ci.org/alumbra/alumbra.analyzer.svg?branch=master)](https://travis-ci.org/alumbra/alumbra.analyzer)
[![Clojars Project](https://img.shields.io/clojars/v/alumbra/analyzer.svg)](https://clojars.org/alumbra/analyzer)

[alumbra-spec]: https://github.com/alumbra/alumbra.spec
[alumbra-parser]: https://github.com/alumbra/alumbra.parser

## Usage

```clojure
(require '[alumbra.analyzer :as analyzer])
```

__Schema Analysis__

Converts a value conforming to `:analyzer/schema` to `:analyzer/analyzed-schema`
which can be used as a base for GraphQL query document validation. This
operation needs to have a parser injected, since internally it will merge
the GraphQL introspection schema into the given one.

```clojure
(analyzer/analyze-schema
  alumbra.parser/parse-schema
  "type Person { ... } ...")
```

Note that this operation assumes that the input schema has been validated and
is thus semantically sound.

__Canonical Operation Generation__

Based on an analyzed schema, a value conforming to `:analyzer/document` can
be converted to `:analyzer/canonical-operation` which is a self-contained
format suitable for execution.

```clojure
(analyzer/canonicalize-operation
  analyzed-schema
  (alumbra.parser/parse-document "{ me { name } }"))
```

Note that this operation assumes that the input document has been validated and
is thus semantically sound.

## Issues

Issue tracking for alumbra is centralized at [alumbra/alumbra][issues].

[issues]: https://github.com/alumbra/alumbra/issues

## License

```
MIT License

Copyright (c) 2016 Yannick Scherer

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```
