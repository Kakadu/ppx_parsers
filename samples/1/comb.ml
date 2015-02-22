open Printf

module type STREAM = sig
   type t
   val create: string -> t
   val look: t -> string -> t option (* it handles whitespace *)
   val is_finished: t -> bool
   (*val forward: t -> int -> t*)
   (* TODO: add memoize maybe *)
   val position: t -> int
end

module type PARSERS = sig
    type stream
    type err
    type 'rez r = Parsed of 'rez * err * stream | Failed of err
    type 'a parser = stream -> 'a r

    val const: 'a -> 'b -> 'a
    val map_result: ('a -> 'b) -> 'a r -> 'b r
    val look: string -> stream -> string r
    val (<|>): 'a parser -> 'a parser -> 'a parser
    val alt_list: 'a parser list -> 'a parser
    val (-->): 'a parser -> ('a -> 'b) -> 'b parser
    val opt: 'a parser -> 'a option parser
    val lift : stream -> unit r
    val (>>=): 'a parser -> ('a -> 'b parser) -> 'b parser
    val (@~>): 'a parser -> 'b parser -> 'b parser
    val (<~@): 'a parser -> 'b parser -> 'a parser
    val many_fold: ('init -> 'a -> 'init) -> 'init -> 'a parser -> 'init parser
    val number: int parser
    val list0:  'a parser -> 'b parser -> 'a list parser
    val listBy: 'a parser -> 'b parser -> 'a list parser
    val whitespaces: unit parser

end

module Make(S: STREAM) : PARSERS with type stream = S.t and type err = unit =
struct
  type stream = S.t
  type err = unit
  type 'a r = Parsed of 'a * err * stream | Failed of err
  type 'a parser = S.t -> 'a r

  let const x _ = x
  let map_result f = function Parsed (x, (), s) -> Parsed (f x, (), s) | Failed () -> Failed ()
  let look str t =
    match S.look t str with
    | Some t -> printf "look '%s' succeeded\n%!" str; Parsed (str, (), t)
    | None -> printf "look '%s' failed\n%!" str; Failed ()

  let (<|>) l r s =
    match l s with
    | (Parsed (_,_,_)) as ans -> ans
    | Failed () -> r s

  let alt_list : 'a parser list -> 'a parser = function
    | []    -> failwith "alt_list can't accpet empty list"
    | x::xs ->  List.fold_left (<|>) x xs

  let (-->) p f s = match p s with Parsed (x,(),s) -> Parsed (f x, (), s) | Failed () -> Failed ()

  let opt p s =
    match p s with
    | Parsed (v,(),s') -> Parsed (Some v, (), s')
    | Failed ()     -> Parsed (None, (), s)

  let lift s = Parsed ((),(),s)

  let seq p1 p2 s =
    match p1 s with
    | Parsed (x,(),s') -> p2 x s'
    | Failed () -> Failed ()

  (* let (|>) = seq *)
  let (>>=) = seq

  let (@~>) p1 p2 s : _ r =
    match p1 s with
    | Parsed (_, (), s') -> p2 s'
    | Failed () -> Failed ()

  let (<~@) p1 p2 s =
    match p1 s with
    | Parsed (ans, (), s') -> (p2 --> (fun _ -> ans)) s'
    | Failed () -> Failed ()

  let many_fold f init p =
    let rec inner acc s =
      match p s with
      | Parsed (x, (), s') -> inner (f acc x) s'
      | Failed ()      -> Parsed (acc, (), s)
    in
    inner init

  let many p =
    (many_fold (fun acc x -> fun l -> acc (x::l)) (fun x -> x) p) --> (fun t -> t [])

  let list0 p delim =
    (p  >>= fun h ->
        many (delim >>= const p) --> fun tl ->  (h::tl)
    )
    <|>
      (lift --> fun _ -> [])

  let listBy p delim : _ list parser =
    p >>= fun h ->
    many (delim @~> p) --> fun tl -> h::tl

  let digit t : int r =
    let f acc n : _ parser = acc <|> (look (string_of_int n)) in
    let g = List.fold_left f (look "0") [1;2;3;4;5;6;7;8;9] in
    map_result int_of_string (g t)

  let many1fold f p =
    let rec helper acc t =
      match p t with
      | Parsed (x, (), t) -> helper (f acc x) t
      | Failed ()    -> Parsed (acc, (), t)
    in
    p >>= helper

  let number = many1fold (fun acc x -> acc*10+x) digit

  let whitespace : string parser = (look " ") <|> (look "\n") <|> (look "\t")
  let whitespaces = many_fold (fun () _ -> ()) () whitespace


end
