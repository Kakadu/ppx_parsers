
module ASDF1  = struct
  include Comb.Make(Lexer.SimpleStream)
  module Lexer = Lexer.SimpleStream

  (* let h =  (look "true") --> ( (^)"1") [@@parser] *)

    let h _stream =
      let error = ref "" in
      let ans = ref ((Lexer.create ""), (Obj.magic ())) in
      let () =
        let _ans2 = ref ((Lexer.create ""), (Obj.magic ())) in
        let _ = "call left there" in
        let () =
          match Lexer.look _stream "true" with
          | Some stream2 -> _ans2 := (stream2, "true")
          | None  -> error := (Printf.sprintf "can't parse '%s'" "true") in
        if (!error) = ""
        then ans := ((fst (!_ans2)), (((^) "1") @@ (snd (!_ans2)))) in
      let error = !error in
      if error = ""
      then Parsed ((snd (!ans)), (), (fst (!ans)))
      else Failed ()

end [@@parsers]
