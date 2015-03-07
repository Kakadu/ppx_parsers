
module ASDF1  = struct
    open Comb

    let u p1 p2 s =
      match p1 s with
      | (Parsed (ans,(),s') as x) -> x
      | Failed () -> p2 s

    let d = fun s -> match look "a" s with
                     | (Parsed _) as x -> x
                     | Failed _ -> look "b" s

    let e = (look "a") <|> (look "b") [@@parser]

    (* let rec boo x = foo x *)
    (* and foo x = boo x *)

end[@@parser]
