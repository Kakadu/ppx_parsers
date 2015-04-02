(* Big JSON parser should be there *)

type json =
  | Number of float
  | Null
  | BoolLit of bool
  | StringLit of string
  | Obj of (string * json) list
  | Arr of json list

module ASDF1  = struct
  include Comb.MakeExt(TestLexer.SimpleStream)
  module Lexer = TestLexer.SimpleStream (* TODO: move it to functor *)

  let rec value _stream: json r =
    (wss @~> (obj <|> arr <|>
              (float_number --> (fun f -> Number f))     <|>
              (look "true"  --> (fun _ -> BoolLit true))  <|>
              (look "false" --> (fun _ -> BoolLit false)) <|>
              (look "null" --> (fun _ -> Null))
             )
    ) _stream
      [@@parser]
  and obj _stream =
    ( ((wss @~> look "{") @~> (list0By member (look ",")) <~@ (wss @~> look "}")) --> (fun xs -> Obj xs) ) _stream
      [@@parser]
  and arr _stream =
    ((wss @~> look "[" @~> (list0By value (look ",")) <~@ (wss @~> look "]") )--> (fun xs -> Arr xs))  _stream
    [@@parser]
  and member _stream: (string*json) r = (wss @~> string_lit <@> (wss @~> (look ":") @~> wss @~> value)) _stream
      [@@parser]


end [@@parsers]
