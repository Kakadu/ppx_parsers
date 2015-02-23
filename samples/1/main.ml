open Printf

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

let () =
  let s = "12*2+3" in
  printf "input: %s\n%!" s;
  let stream = SimpleStream.create s in
  let open Expr1 in
  match expr stream with
  | Parsed (ans,(),_) -> printf "Parsed: %d\n%!" ans
  | Failed () -> printf "Failed\n%!"; exit 1

module Expr2 = struct
    include Comb.Make(SimpleStream)

    type ast = Val of int | Add of ast*ast | Mul of ast*ast | Sub of ast*ast | Div of ast*ast
    let string_of_ast x =
      let b = Buffer.create 100 in
      let add_string = Buffer.add_string b in
      let rec loop = function
        | Val n -> Buffer.add_string b (string_of_int n)
        | Add (l,r) -> add_string "("; loop l; add_string "+"; loop r; add_string ")"
        | Mul (l,r) -> loop l; add_string "*"; loop r
        | Sub (l,r) -> add_string "("; loop l; add_string "-"; loop r; add_string ")"
        | Div (l,r) -> loop l; add_string "/"; loop r
      in
      loop x;
      Buffer.contents b

    let num = number --> fun x -> Val x

    open Comb
    let e =
      expr (fun x -> x)
           [| `Lefta , [look ("+"), (fun x y -> Add (x, y)); look ("-"), (fun x y -> Sub (x, y)) ]
            ; `Lefta , [look ("*"), (fun x y -> Mul (x, y)); look ("/"), (fun x y -> Div (x, y)) ]
           |]
           num

    (* let rec expr t = (( *)
    (*   num    >>= fun left -> print_endline "here1"; *)
    (*   op     >>= fun op  -> *)
    (*   expr --> fun right -> op left right) <|> num) t *)

end

let () =
  let s = "1+2*3-4" in
  printf "input: %s\n%!" s;
  let stream = SimpleStream.create s in
  let open Expr2 in
  match e stream with
  | Parsed (ans,(),_) -> printf "Parsed: %s\n%!" (string_of_ast ans)
  | Failed () -> printf "Failed\n%!"; exit 1
