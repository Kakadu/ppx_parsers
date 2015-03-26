open Printf

module Pos = struct
  type t = int
  let compare: int -> int -> int = compare
end

module type ERRMAP = sig
    include Map.S with type key = Pos.t
    type param = string list
    val merge_errs: param t -> param t -> param t
    val my_create: string -> key -> string list t
    val to_string: param t -> string
end

module ErrMap : ERRMAP = struct
  include Map.Make(Pos)
  type param = string list
  let merge_errs =
    merge (fun pos l r ->
           match l,r with
           | Some x, Some y -> Some (x@y)
           | None, y -> y
           | x, None -> x
          )
  let my_create msg pos = add pos [msg] empty
  let to_string m =
    let b = Buffer.create 100 in
    iter (fun key -> List.iter (fun s -> Buffer.add_string b @@ sprintf "%d: %s\n" key s)) m;
    Buffer.contents b
end



open Comb

module Make(S: STREAM) : PARSERS with type stream = S.t and type err = ErrMap.param ErrMap.t =
struct
  type stream = S.t
  type err = ErrMap.param ErrMap.t
  type 'a r = Parsed of 'a * err * stream | Failed of err
  type 'a parser = S.t -> 'a r

  let add_err_r err = function
    | Parsed (x, e', s) -> Parsed (x, ErrMap.merge_errs e' err, s)
    | Failed e' -> Failed (ErrMap.merge_errs e' err)

  let const x _ = x
  let map_result f = function Parsed (x,err,s) -> Parsed (f x,err,s) | Failed x -> Failed x
  let look patt t =
    match S.look t patt with
    | Some t -> (* printf "look '%s' succeeded\n%!" patt; *)
                Parsed (patt, ErrMap.empty, t)
    | None -> (* printf "look '%s' failed\n%!" patt; *)
              Failed (ErrMap.my_create (sprintf "can't parse '%s'" patt) (S.position t))

  let (<|>) l r s =
    match l s with
    | (Parsed (_,_,_)) as ans -> ans
    | Failed err -> add_err_r err (r s)

  let alt_list : 'a parser list -> 'a parser = function
    | []    -> failwith "alt_list can't accpet empty list"
    | x::xs ->  List.fold_left (<|>) x xs

  let (-->) p f s = match p s with Parsed (x,e,s) -> Parsed (f x, e, s) | Failed e -> Failed e

  let opt p s =
    match p s with
    | Parsed (v,e,s') -> Parsed (Some v,e,s')
    | Failed e     -> Parsed (None, e, s)

  let lift s = Parsed ((), ErrMap.empty, s)

  let seq p1 p2 s =
    match p1 s with
    | Parsed (x,e,s') -> add_err_r e (p2 x s')
    | Failed e -> Failed e

  let (>>=) = seq

  let (@~>) p1 p2 s : _ r =
    match p1 s with
    | Parsed (_,e,s') -> add_err_r e (p2 s')
    | Failed e -> Failed e

  let (<~@) p1 p2 s =
    match p1 s with
    | Parsed (ans,e1,s') -> begin
        match p2 s' with
        | Parsed (_, e2, s) -> add_err_r e1 (Parsed(ans,e2,s))
        | Failed e -> Failed (ErrMap.merge_errs e1 e)
      end
    | Failed e -> Failed e

  let many_fold f init p =
    let rec inner acc eacc s =
      match p s with
      | Parsed (x, e, s') -> inner (f acc x) (ErrMap.merge_errs eacc e) s'
      | Failed e          -> Parsed (acc, ErrMap.merge_errs eacc e, s)
    in
    inner init ErrMap.empty

  let many p =
    (many_fold (fun acc x -> fun l -> acc (x::l)) (fun x -> x) p) --> (fun t -> t [])

  let many1 p =
    p >>= fun h -> many p -->(fun tl -> h::tl)

  let list0By p delim =
    (p  >>= fun h ->
             many (delim >>= const p) --> fun tl -> (h::tl)
    )
    <|>
      (lift --> (fun _ -> print_endline "list0 success"; []) )

  let listBy p delim : _ list parser =
    p >>= fun h ->
    many (delim @~> p) --> fun tl -> h::tl

  let digit t : int r =
    let f acc n : _ parser = acc <|> (look (string_of_int n)) in
    let g = List.fold_left f (look "0") [1;2;3;4;5;6;7;8;9] in
    map_result int_of_string (g t)

  let many1fold f p =
    let rec helper eacc acc t =
      match p t with
      | Parsed (x,e,t) -> helper (ErrMap.merge_errs eacc e) (f acc x) t
      | Failed e    -> Parsed (acc, e, t)
    in
    p >>= helper ErrMap.empty

  let number =
    let body = many1fold (fun acc x -> acc*10+x) digit in
    (look "-" @~> body --> (fun x -> -x)) <|> body

  let whitespace : string parser = (look " ") <|> (look "\n") <|> (look "\t")
  let whitespaces = many_fold (fun () _ -> ()) () whitespace
  let wss = whitespaces

  let expr = assert false
end
