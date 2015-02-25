
module ASDF1  = struct
    open Comb

    let e = (look "a") <|> (look "b") [@@parser]
    (* let rec boo x = foo x *)
    (* and foo x = boo x *)

end[@@parser]

