open TstJSON
open Core.Std
open Core_bench.Std
open Printf

let () =
  let small_test_count = 2 in
  let small_inputs = Array.init small_test_count ~f:(fun n ->
      let filename = sprintf "../../scala/test1/src/test/resources/json%d" (n+1) in
      In_channel.read_all filename
    )
  in
  let big_inputs_count = 8 in

  let small_tests = Array.to_list @@ Array.init small_test_count ~f:(fun n ->
      let name = sprintf "json%d" (n+1) in
      let open TstJSON.ASDF1 in
      let stream = Lexer.create small_inputs.(n) in
      let ans: _ ref = ref (Failed ()) in
      Bench.Test.create ~name (fun () -> ans:= ASDF1.arr stream)
    )
  in

  Bench.(bench ~save_to_file:(fun m -> sprintf "%s.rez" (Measurement.name m) )
           ~display_config:(Display_config.create ~don't_display_table:true ~show_percentage:false ~ascii_table:false ())
           small_tests)
  (* let s = *)
  (*   let filename = "../../scala/test1/src/test/resources/json.big1" in *)
  (*   (\* let filename = "json1" in *\) *)
  (*   let ch = open_in filename in *)
  (*   let ans = ExtLib.input_all ch in *)
  (*   let ()  = close_in ch in *)
  (*   ans *)
  (* in *)
  (* printf "input length: '%d'\n%!" (String.length s); *)
  (* let open ASDF1 in *)
  (* let stream = Lexer.create s in *)
  (* let ans: _ ref = ref (Failed ()) in *)
  (* let () = Core.Std.Command.run (Bench.make_command *)
  (*   [ Bench.Test.create ~name:"Json1" (fun () -> *)
  (*         ans := ASDF1.arr stream *)
  (*       ) *)
  (*   ; Bench.Test.create ~name:"Json1_orig" (fun () -> *)
  (*         ans := ASDF1_orig.arr stream *)
  (*       ) *)
  (*   ] *)
  (* ) *)
  (* in *)

  (* match !ans with *)
  (* | Parsed (_,(),_) -> printf "Parsed successfully \n%!" *)
  (* | Failed _ -> printf "Parsing failed\n%!"; exit 1 *)



(* let () = main () *)
