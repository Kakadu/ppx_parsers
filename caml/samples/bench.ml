open Printf
open TstJSON
open Core_bench.Std

let () =
  let s =
    let filename = "../../scala/test1/src/test/resources/json.big1" in
    (* let filename = "json1" in *)
    let ch = open_in filename in
    let ans = ExtLib.input_all ch in
    let ()  = close_in ch in
    ans
  in
  printf "input length: '%d'\n%!" (String.length s);
  let open ASDF1 in
  let stream = Lexer.create s in
  let ans: _ ref = ref (Failed ()) in
  let () = Core.Std.Command.run (Bench.make_command
    [ Bench.Test.create ~name:"Json1" (fun () ->
          ans := ASDF1.arr stream
        )
    ; Bench.Test.create ~name:"Json1" (fun () ->
          ans := ASDF1_orig.arr stream
        )
    ]
  )
  in

  match !ans with
  | Parsed (_,(),_) -> printf "Parsed successfully \n%!"
  | Failed _ -> printf "Parsing failed\n%!"; exit 1


open Core.Std
let main () =
  Random.self_init ();
  (* let x = Random.float 10.0 in *)
  (* let y = Random.float 10.0 in *)
  Command.run (Bench.make_command [
    (* Bench.Test.create ~name:"Float add" (fun () -> *)
    (*   ignore (x +. y)); *)
    (* Bench.Test.create ~name:"Float mul" (fun () -> *)
    (*   ignore (x *. y)); *)
    (* Bench.Test.create ~name:"Float div" (fun () -> *)
    (*   ignore (x /. y)); *)
  ])

(* let () = main () *)
