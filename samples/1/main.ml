
module SimpleStream : Comb.STREAM = struct
 type t = int*string
 let create s : t = (0,s)
 let is_finished (n,s) = String.length s >= n
 let position = fst
 let look (n,s) patt =
   let pattlen = String.length patt in
   if n + pattlen > String.length s then None
   else begin
       let rec checker count pattpos inputpos =
         if count = 0 then true
         else if s.[inputpos] = patt.[pattpos] then checker (count-1) (pattpos+1) (inputpos+1)
         else false
       in
       if checker pattlen 0 n then Some (n+pattlen, s) else None
     end
end

module Expr1 = struct


    include Comb.Make(SimpleStream)
    let op =
      (look "+" --> (fun _ -> (+))) <|>
      (look "-" --> (fun _ -> (-))) <|>
      (look "*" --> (fun _ -> ( * ))) <|>
      (look "/" --> (fun _ -> (/)))

    let num = number
    let rec expr t = ((
      num    >>= fun left -> print_endline "here1";
      op     >>= fun op  ->
      expr --> fun right -> op left right) <|> num) t

end

open Printf
let () =
  let s = "12*2+3" in
  printf "input: %s\n%!" s;
  let stream = SimpleStream.create s in
  let open Expr1 in
  match expr stream with
  | Parsed (ans,(),_) -> printf "Parsed: %d\n%!" ans
  | Failed () -> printf "Failed\n%!"; exit 1
