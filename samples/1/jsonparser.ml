open Printf

module SimpleStream  = struct
 type t = int*string
 let create s : t = (0,s)
 let is_finished (n,s) = String.length s >= n
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

 let string_literal ((cur_pos,s) as stream) =
   if is_finished stream then None else
   let input_len = String.length s in
   let rec loop last_char pos =
     if pos >=input_len then -1 else
     if s.[pos] = '"' && last_char = '\\' then loop s.[pos] (pos+1) else
     if s.[pos] = '"' then pos
     else loop s.[pos] (pos+1)
   in
   if s.[cur_pos] <> '"' then None else
   let r = loop '"' (cur_pos+1) in
   if r = -1 then None
   else Some (StringLabels.sub s ~pos:cur_pos ~len:(1+r-cur_pos), (r+1,s) )
end


module Json1 = struct
    include Comb.Make(SimpleStream)

    let debug_map msg p stream =
      let ans = p stream in
      match p stream with
      | Parsed (_,_) -> printf "%s +\n%!" msg; ans
      | Failed       -> printf "%s -\n%!" msg; ans

    let string_lit s : string r =
      match SimpleStream.string_literal (Obj.magic s) with
      | Some (s, stream) -> Parsed (s, stream)
      | None -> Failed

    let rec value stream : unit r =
      (whitespaces @~>
         (alt_list [ obj --> const ()
                   ; arr --> const ()
                   ; string_lit --> const ()
                   ; number --> const ()
                   ; ((look "null") <|> (look "true") <|> (look "false")) --> const ()
      ])) stream

    and obj s: _ list r = ((look "{") @~> (listBy member (look ",") |> (debug_map "listByObj") ) <~@ (look "}")) s
    and arr s: _ list r =
      print_endline "Calling arr";
      ((look "[") @~> ( (listBy member (look ",")) |> (debug_map "listByArr") )  <~@ (look "]")) s
    and member s: unit r = ((string_lit @~> (look ":") @~> value) |> (debug_map "member")) s


end

let () =
  let s =
    let ch = open_in "json0" in
    let ans = Std.input_all ch in
    close_in ch;
    ans
  in
  printf "input length: %d\n%!" (String.length s);
  let stream = SimpleStream.create s in
  let open Json1 in
  match arr stream with
  | Parsed (_,_) -> printf "Parsed\n%!"
  | Failed -> printf "Failed\n%!"; exit 1


