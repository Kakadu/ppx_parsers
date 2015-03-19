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
