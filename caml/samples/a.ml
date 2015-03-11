
module ASDF1  = struct
    open Comb
(*
    let u p1 p2 s =
      match p1 s with
      | (Parsed (ans,(),s') as x) -> x
      | Failed () -> p2 s
*)
   let f = (look "true") [@@parser]


    (* let f = (look "true") <|> (look "false") [@@parser] *)

end[@@parser]
