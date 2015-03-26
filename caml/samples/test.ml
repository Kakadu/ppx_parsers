open Printf
open OUnit2
(*
let wrap_call_f ~input should_success =
  let new_ =
    let open A.ASDF1 in
    let stream = Lexer.create input in
    main stream
  in
  let old_ =
    let open A.ASDF1_orig in
    let stream = Lexer.create input in
    main stream
  in
  let () = if should_success then begin
               assert_bool "<no message>" (match new_ with A.ASDF1.Parsed _ -> true | _ -> false);
               assert_bool "<no message>" (match old_ with A.ASDF1_orig.Parsed _ -> true | _ -> false);
               match new_,old_ with
               | (A.ASDF1.Parsed (r1,_,_), A.ASDF1_orig.Parsed (r2,_,_)) ->
                  assert_equal ~msg:"Success results should be equal" r1 r2
               | _ -> ()
             end else begin
               assert_bool "<no message>" (match new_ with A.ASDF1.Parsed _ -> false | _ -> true);
               assert_bool "<no message>" (match old_ with A.ASDF1_orig.Parsed _ -> false | _ -> true);
             end
  in
  ()

let wrap_call_g ~input should_success =
  let new_ =
    let open A.ASDF1 in
    let stream = Lexer.create input in
    main stream
  in
  let old_ =
    let open A.ASDF1_orig in
    let stream = Lexer.create input in
    main stream
  in
  let () = if should_success then begin
               assert_bool "<no message>" (match new_ with A.ASDF1.Parsed _ -> true | _ -> false);
               assert_bool "<no message>" (match old_ with A.ASDF1_orig.Parsed _ -> true | _ -> false);
               match new_,old_ with
               | (A.ASDF1.Parsed (r1,_,_), A.ASDF1_orig.Parsed (r2,_,_)) ->
                  assert_equal ~msg:"Success results should be equal" r1 r2
               | _ -> ()
             end else begin
               assert_bool "<no message>" (match new_ with A.ASDF1.Parsed _ -> false | _ -> true);
               assert_bool "<no message>" (match old_ with A.ASDF1_orig.Parsed _ -> false | _ -> true);
             end
  in
  ()
  *)
module type TEST_PARSER = sig
  type stream
  type err
  type 'a r = Parsed of 'a * err * stream | Failed of err
  type 'a parser = stream -> 'a r

  module Lexer : sig val create: string -> stream end
  val main: string parser
end

let magic_wrap ~input should_success (module New: TEST_PARSER) (module Orig: TEST_PARSER) =
  let new_ =
    let stream = New.Lexer.create input in
    New.main stream
  in
  let old_ =
    let stream = Orig.Lexer.create input in
    Orig.main stream
  in
  let () = if should_success then begin
               assert_bool "<no message>" (match new_ with New.Parsed _ -> true | _ -> false);
               assert_bool "<no message>" (match old_ with Orig.Parsed _ -> true | _ -> false);
               match new_,old_ with
               | (New.Parsed (r1,_,_), Orig.Parsed (r2,_,_)) ->
                 (* print_endline ""; *)
                 (* print_endline r1; *)
                 (* print_endline r2; *)
                 assert_equal ~msg:"Success results should be equal" r1 r2
               | _ -> ()
             end else begin
               assert_bool "<no message>" (match new_ with New.Parsed _ -> false | _ -> true);
               assert_bool "<no message>" (match old_ with Orig.Parsed _ -> false | _ -> true);
             end
  in
  ()

let suite =
  "allfuns" >:::
  [ "dummy" >:: (fun _ -> ())
  ; "0">:: (fun _ctx ->
             let module L = TestLexer.SimpleStream in
             let l = L.create "" in assert_bool "Lexer.is_finished" (L.is_finished l) )
  ; "1">:: (fun _ctx ->  magic_wrap ~input:"true"  true  (module A.ASDF1) (module A.ASDF1_orig) )
  ; "2">:: (fun _ctx ->  magic_wrap ~input:"tru1e" false (module A.ASDF1) (module A.ASDF1_orig) )

  ; "3">:: (fun _ctx ->  magic_wrap ~input:"true"  true  (module B.ASDF1) (module B.ASDF1_orig) )
  ; "4">:: (fun _ctx ->  magic_wrap ~input:"tru1e" false (module B.ASDF1) (module B.ASDF1_orig) )

  ; "alternative1">:: (fun _ctx ->  magic_wrap ~input:"true"  true (module C.ASDF1) (module C.ASDF1_orig) )
  ; "alternative2">:: (fun _ctx ->  magic_wrap ~input:"false" true (module C.ASDF1) (module C.ASDF1_orig) )

  ; "SimpleMany">:: (fun _ ->  magic_wrap true (module D.ASDF1) (module D.ASDF1_orig) ~input:"true true true" )

  ; "Oexpr simple">:: (fun _ ->  magic_wrap true (module F.ASDF1) (module F.ASDF1_orig) ~input:"{x}" )

  ; "WS">::         (fun _ ->  magic_wrap true (module G.ASDF1) (module G.ASDF1_orig) ~input:" false true" )
  ; "jsonLike1">:: (fun _ -> magic_wrap true  (module H.ASDF1) (module H.ASDF1_orig) ~input:"{true}"  )
  ; "jsonLike2">:: (fun _ -> magic_wrap true  (module H.ASDF1) (module H.ASDF1_orig) ~input:"{false}")
  ; "jsonLike3">:: (fun _ -> magic_wrap true  (module H.ASDF1) (module H.ASDF1_orig) ~input:"{true,false}")
  ; "jsonLike4">:: (fun _ -> magic_wrap true  (module H.ASDF1) (module H.ASDF1_orig) ~input:"{false,true}")

  ; "EBNF+">:: (fun _ -> magic_wrap true  (module I.ASDF1) (module I.ASDF1_orig) ~input:"true false false")
  ; "EBNF+">:: (fun _ -> magic_wrap false (module I.ASDF1) (module I.ASDF1_orig) ~input:"false")
  ]

let () = run_test_tt_main suite
