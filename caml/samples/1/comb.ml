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

type associvity = [ `Lefta | `Righta | `Nona ]


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
    val many: 'a parser -> 'a list parser
    val number: int parser
    val list0:  'a parser -> 'b parser -> 'a list parser
    val listBy: 'a parser -> 'b parser -> 'a list parser
    val whitespaces: unit parser

    val expr:  ('a parser -> 'a parser) ->
               ([ `Lefta | `Nona | `Righta ] * ('oper parser * ('a -> 'a -> 'a)) list) array ->
               'a parser -> 'a parser
end


let left f c x y = f (c x) y
let right f c x y = f x y

module Make(S: STREAM) : PARSERS with type stream = S.t and type err = unit =
struct
  type stream = S.t
  type err = unit
  type 'a r = Parsed of 'a * err * stream | Failed of err
  type 'a parser = stream -> 'a r

  let const x _ = x
  let map_result f = function Parsed (x, (), s) -> Parsed (f x, (), s) | Failed () -> Failed ()
  let look str t =
    match S.look t str with
    | Some t -> Parsed (str, (), t)
    | None   -> Failed ()

  let (<|>) l r s =
    match l s with
    | (Parsed (_,_,_)) as ans -> ans
    | Failed () -> r s

  let alt = (<|>)
  let alt_list : 'a parser list -> 'a parser = function
    | []    -> failwith "alt_list can't accpet empty list"
    | x::xs -> List.fold_left (<|>) x xs

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

  let map f p = p --> f
  let empty s = Parsed ((), (), s)
  let guard p f r s =
    let x = p s in
    match x with
    | Parsed (ans, (), s) ->  if f ans then x else Failed (match r with None -> () | Some r -> r ans)
    | Failed () -> Failed ()
(*
 let  e = expr id
               [| left , [ostap ("+"), (fun x y -> `Add (x, y)); ostap ("-"), (fun x y -> `Sub (x, y))]
                ; left , [ostap ("*"), (fun x y -> `Mul (x, y)); ostap ("/"), (fun x y -> `Div (x, y))]
               |]
               primary
               s
 *)

  let expr:   ('a parser -> 'a parser) ->
               ([ `Lefta | `Nona | `Righta ] * ('oper parser * ('a -> 'a -> 'a)) list) array ->
               'a parser -> 'a parser
  = fun f ops opnd ->
    let ops = ops |> Array.map (fun (assoc, lst) ->
                                let g = match assoc with `Lefta | `Nona -> left | `Righta -> right in
                                assoc = `Nona, alt_list (List.map (fun (oper, action) -> oper --> fun _ -> g action) lst)
                               )
    in
    let (_: (bool * (('b -> 'b) -> 'b -> 'c -> 'd) parser) array) = ops in
    let n = Array.length ops in
    let op i   = snd ops.(i) in
    let nona i = fst ops.(i) in
    let id x = x in


    let rec inner l c =
       f
         (fun _ostap_stream ->
            alt
              (seq
                 (guard empty
                    (fun _ -> n = l) None)
                 (fun _ -> map (fun (x as _0) -> c x) opnd))
              (alt
                 (seq
                    (guard empty
                       (fun _ -> n > l && not (nona l)) None)
                    (fun _ ->
                       seq
                         (fun _ostap_stream -> inner (l + 1) id _ostap_stream)
                         (fun (x as _1) ->
                            map
                              (fun (b as _0) ->
                                 match b with
                                   None -> c x
                                 | Some x -> x)
                              (opt
                                 (seq
                                    (fun _ostap_stream -> op l _ostap_stream)
                                    (fun o _ostap_stream ->
                                       inner l (o c x) _ostap_stream))))))
                 (seq
                    (guard empty
                       (fun _ -> n > l && nona l) None)
                    (fun _ ->
                       seq
                         (fun _ostap_stream -> inner (l + 1) id _ostap_stream)
                         (fun (x as _1) ->
                            map
                              (fun (b as _0) ->
                                 match b with
                                   None -> c x
                                 | Some x -> x)
                              (opt
                                 (seq
                                    (fun _ostap_stream -> op l _ostap_stream)
                                    (fun o _ostap_stream ->
                                       inner (l + 1) (o c x)
                                         _ostap_stream)))))))
              _ostap_stream)
    in
    inner 0 id


end
