open TstJSON
open Core_bench
open Core_bench.Std
open Core.Std
open Printf

type filename = string
type test_item =
  { ti_filename: filename
  ; ti_content:  string
  ; mutable ti_time: float
  ; mutable ti_time_opt: float
  ; mutable ti_time_scala: float
  }
let make_test_item ti_filename ti_content = { ti_filename; ti_content; ti_time=0.0; ti_time_opt=0.0; ti_time_scala=0.0 }
type test_category =
  { title: string
  ; all_inputs: test_item array
  ; filename: string  (* result file name *)
  ; quota: float
  }


let print_category cat =
  let ch = open_out cat.filename in
  fprintf ch "Name PC PC_opt PC_scala PC_scala_opt\n";
  Array.iter cat.all_inputs ~f:(fun {ti_filename; ti_time; ti_time_opt; ti_time_scala} ->
      fprintf ch "%s %f %f %f %f\n" ti_filename ti_time_opt ti_time ti_time_scala 0.0
    );
  Out_channel.close ch

let () =

  let small_cat =
    let title = "SmallTests" in
    let small_test_count = 20 in
    let all_inputs = Array.init small_test_count ~f:(fun n ->
        let filename = sprintf "../../scala/test1/src/test/resources/json%d" (n+1) in
        make_test_item (Filename.basename filename) (In_channel.read_all filename)
      )
    in
    let filename = "small.rez" in
    { title; all_inputs; filename; quota=2.0 }
  in

  let big_cat =
    let title = "BigTests" in
    let small_test_count = 9 in
    let all_inputs = Array.init small_test_count ~f:(fun n ->
        let filename = sprintf "../../scala/test1/src/test/resources/json%d" (n+1) in
        make_test_item (Filename.basename filename) (In_channel.read_all filename)
      )
    in
    let filename = "big.rez" in
    { title; all_inputs; filename; quota=10.0 }
  in

  let verybig_cat =
    let title = "BigTests" in
    let small_test_count = 1 in
    let all_inputs = Array.init small_test_count ~f:(fun _n ->
        let filename = "../../scala/test1/src/test/resources/json.vbig" in
        make_test_item (Filename.basename filename) (In_channel.read_all filename)
      )
    in
    let filename = "vbig.rez" in
    { title; all_inputs; filename; quota=10.0 }
  in

  let all_categories = [ small_cat; big_cat; verybig_cat ] in

  List.iter all_categories ~f:(fun ({title; all_inputs; filename; quota} as cat) ->

      (* let put_result,close_results = *)
      (*   let ch = open_out filename in *)
      (*   let put ~name time = Printf.fprintf ch "%s,%s\n%!" name time in *)
      (*   let close () = Out_channel.close ch in *)
      (*   (put,close) *)
      (* in *)

      Array.iter all_inputs ~f:(fun test_item ->
          let tests =
            [ begin
              let open TstJSON.ASDF1 in
              let stream = Lexer.create test_item.ti_content in
              let ans: _ ref = ref (Failed ()) in
              Bench.Test.create ~name:test_item.ti_filename (fun () -> ans:= ASDF1.arr stream)
            end
            ; begin
              let open TstJSON.ASDF1_orig in
              let stream = Lexer.create test_item.ti_content in
              let ans: _ ref = ref (Failed ()) in
              Bench.Test.create ~name:test_item.ti_filename (fun () -> ans:= ASDF1.arr stream)
            end
            ]
          in
          let meass = Bench.measure ~run_config:(Bench.Run_config.create ~time_quota:(Time.Span.of_sec quota) ()) tests in
          let results = List.map ~f:Bench.analyze meass in
          let results = List.filter_map results ~f:(function
              | Error err ->
                printf "Error %s\n%!" (Error.to_string_hum err);
                None
              | Ok r -> Some r)
          in
          (* Bench.display results; *)
          assert (List.length results = 2);
          let get_time_exn ar =
            let open Core_bench.Analysis_result in
            let time_results =
              Array.to_list (regressions ar) |> List.filter_map ~f:(fun r ->
                  match Variable.to_string @@ Regression.responder r with
                  | "Time" ->
                    let coef = Regression.coefficients r in
                    let sum = Array.to_list coef
                              |> List.fold_left ~init:0.0 ~f:(fun acc x -> acc +. (Coefficient.estimate x) ) in
                    Some (sum /. (Float.of_int @@ Array.length coef))
                  | _      -> None
                )
            in
            assert (List.length time_results = 1);
            List.hd time_results
          in
          test_item.ti_time_opt <- Option.value_exn (get_time_exn @@ List.nth_exn results 0);
          test_item.ti_time     <- Option.value_exn (get_time_exn @@ List.nth_exn results 1);
        );
        print_endline "category finished";
        print_category cat
    )
