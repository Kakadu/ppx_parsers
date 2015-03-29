open Printf
let log fmt = kprintf (printf "=== %s\n%!") fmt


module SimpleStream : Comb.STREAM = struct
 type t = int*string
 let create s : t = (0,s)
 let is_finished (n,s) = n >= String.length s
 let position = fst
 let look (n,s) patt =
   (* log "inside look for '%s' from pos %d in '%s'" patt n s; *)
   let pattlen = String.length patt in
   if n + pattlen > String.length s then None
   else begin
       let rec checker count pattpos inputpos =
         if count = 0 then true
         else if s.[inputpos] = patt.[pattpos] then checker (count-1) (pattpos+1) (inputpos+1)
         else false
       in
       if checker pattlen 0 n
       then
         (* let () = log "look success" in *)
         Some (n+pattlen, s)
       else
         (* let () = log "look failed" in *)
         None
     end

 let rec skip_ws ((n,s) as stream) =
   (* printf "call skip_ws from pos %d\n" n; *)
   if is_finished stream then ((* log "stream is finished"; *) stream)
   else
     let slen = String.length s in
     let rec loop pos =
       (* log "call skip_ws loop from pos %d, slen=%d" pos slen; *)
       if pos < slen then match s.[pos] with ' '|'\n'|'\t' -> loop (pos+1) | _ -> (pos,s)
       else (pos,s)
     in
     let ans = loop n in
     (* log "skip_ws returns on pos %d" (position ans); *)
     ans

end
