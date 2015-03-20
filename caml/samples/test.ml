open Printf
open A
open OUnit2

let wrap_call_f ~input should_success =
  let new_ =
    let open ASDF1 in
    let stream = Lexer.create input in
    f stream
  in
  let old_ =
    let open ASDF1_orig in
    let stream = Lexer.create input in
    f stream
  in
  let () = if should_success then begin
               assert_bool "<no message>" (match new_ with  ASDF1.Parsed _ -> true | _ -> false);
               assert_bool "<no message>" (match old_ with  ASDF1_orig.Parsed _ -> true | _ -> false);
               match new_,old_ with
               | (ASDF1.Parsed (r1,_,_), ASDF1_orig.Parsed (r2,_,_)) ->
                  assert_equal ~msg:"Success results should be equal" r1 r2
               | _ -> ()
             end else begin
               assert_bool "<no message>" (match new_ with  ASDF1.Parsed _ -> false | _ -> true);
               assert_bool "<no message>" (match old_ with  ASDF1_orig.Parsed _ -> false | _ -> true);
             end
  in
  ()

let wrap_call_g ~input should_success =
  let new_ =
    let open ASDF1 in
    let stream = Lexer.create input in
    g stream
  in
  let old_ =
    let open ASDF1_orig in
    let stream = Lexer.create input in
    g stream
  in
  let () = if should_success then begin
               assert_bool "<no message>" (match new_ with  ASDF1.Parsed _ -> true | _ -> false);
               assert_bool "<no message>" (match old_ with  ASDF1_orig.Parsed _ -> true | _ -> false);
               match new_,old_ with
               | (ASDF1.Parsed (r1,_,_), ASDF1_orig.Parsed (r2,_,_)) ->
                  assert_equal ~msg:"Success results should be equal" r1 r2
               | _ -> ()
             end else begin
               assert_bool "<no message>" (match new_ with  ASDF1.Parsed _ -> false | _ -> true);
               assert_bool "<no message>" (match old_ with  ASDF1_orig.Parsed _ -> false | _ -> true);
             end
  in
  ()

let suite =
  "allfuns" >:::
  [ "1">:: (fun _ctx ->  wrap_call_f ~input:"true"  true)
  ; "2">:: (fun _ctx ->  wrap_call_f ~input:"tr2ue" false)
  ; "3">:: (fun _ctx ->  wrap_call_g ~input:"true"  true)
  ; "4">:: (fun _ctx ->  wrap_call_g ~input:"false" true)
  ]

let () = run_test_tt_main suite
