
module ASDF1  = struct
  include Comb.MakeExt(TestLexer.SimpleStream)
  module Lexer = TestLexer.SimpleStream (* TODO: move it to functor *)

  let main = ((float_number <~@ wss) <@> float_number) --> (fun (a,b) -> string_of_float (a +. b) ) [@@parser]

end [@@parsers]
