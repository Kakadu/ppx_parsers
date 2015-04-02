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

  let main = ((float_number <~@ wss) <@> float_number) --> (fun (a,b) -> string_of_float (a +. b) ) [@@parser]

  let rec value: json parser =
    wss @~> (obj <|> arr <|>
             (float_number --> (fun f -> Number f))     <|>
             (look "true"  --> (fun _ -> BoolLit true))  <|>
             (look "false" --> (fun _ -> BoolLit false)) <|>
             (look "null" --> (fun _ -> Null))
            )
      [@@parser]
  and obj = ((look "{") @~> (list0By member (look ",")) <~@ (look "}")) --> (fun xs -> Obj xs)
      [@@parser]
  and arr = (look "[" @~> (list0By value (look ",")) <~@ (look "]") )--> (fun xs -> Arr xs)
      [@@parser]
  and member: (string*json) parser = stringLit <@> (wss @~> (look ":") @~> wss @~> value)
      [@@parser]

(* def value:Parser[Any] =
   whitespaces ~> (obj | arr | stringLit ^^ (_.toString) | decimalNumber | nullValue | trueValue | falseValue) *)
(* def obj:Parser[Any] = '{' ~> repsep(member,comma) <~ closeBracket *)
(* def arr:Parser[Any] = '[' ~> repsep(value,comma) <~ closeSBracket *)
(* def member:Parser[Any] = stringLit ~ (lit(points) ~> value) ^^ {case (a, b) => (a.toString, b)} *)

end [@@parsers]
