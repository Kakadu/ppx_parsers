
[ ] Write PPX extension that do something interesting with OCaml parser combinators.
  [x] Implement minimal vesion which allows to parse JSON both with combinators and generated recursive descent parser.
  [_] Add benchmarks against other parser-combinator libraries: planck and ostap.
  [ ] Support generation of parser with and without error-reporting.
  [ ] Probably we need PPX extension to embed OCaml AST near expression.
[ ] Compare OCaml's naive combinator speed with Scala ones and generated by FastParsers.
  [ ] Understand are the numbers generated by scala benchmark absolute.
[x] Understand the code which is generated by FastParsers
[x] Repo created :)

[Accelerating Parser Combinators with Macros](http://infoscience.epfl.ch/record/200905/files/p7-beguet.pdf) by Eric Béguet (FastParsers author).
https://github.com/begeric/FastParsers/

Original idea from https://github.com/mirage/mirage-www/wiki/Pioneer-Projects .

### Bigarray parser generator

[FastParsers](https://github.com/begeric/FastParsers) is a Scala parser library which uses macros to transform easy-to-write parser combinators into efficient recursive-descent backtracking parsers. The generated parsers are about 20x faster than Scala's parser combinator library even though its interface stay about the same.

An OCaml equivalent that uses Cstruct under the hood to do zero-copy parsing would permit a big speed boost in Mirage's protocol stacks.
