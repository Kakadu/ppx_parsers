open TstJSON
open Core_bench
open Core_bench.Std
open Core.Std
open Printf

type test_category =
  { title: string
  ; all_inputs: (string*string) array
  ; filename: string  (* result file name *)
  ; quota: float
  }


let () =

  let small_cat =
    let title = "SmallTests" in
    let small_test_count = 2 in
    let all_inputs = Array.init small_test_count ~f:(fun n ->
        let filename = sprintf "../../scala/test1/src/test/resources/json%d" (n+1) in
        (Filename.basename filename, In_channel.read_all filename)
      )
    in
    let filename = "small.rez" in
    { title; all_inputs; filename; quota=2.0 }
  in

  let big_inputs_count = 8 in
  let all_categories = [ small_cat ] in

  List.iter all_categories ~f:(fun {title; all_inputs; filename; quota} ->

      let put_result,close_results =
        let ch = open_out filename in
        let put ~name time = Printf.fprintf ch "%s,%s\n%!" name time in
        let close () = Out_channel.close ch in
        (put,close)
      in


      let bench_tests = Array.to_list @@ Array.mapi all_inputs ~f:(fun n (name,input_stream) ->
          let open TstJSON.ASDF1 in
          let stream = Lexer.create input_stream in
          let ans: _ ref = ref (Failed ()) in
          Bench.Test.create ~name (fun () -> ans:= ASDF1.arr stream)
        )
      in

      let meass = Bench.measure ~run_config:(Bench.Run_config.create ~time_quota:(Time.Span.of_sec quota) ()) bench_tests in
      let results = List.map ~f:Bench.analyze meass in
      let results = List.filter_map results ~f:(function
          | Error err ->
            printf "Error %s\n%!" (Error.to_string_hum err);
            None
          | Ok r -> Some r)
      in
      Bench.display results;

      List.iter ~f:(fun (ar: Core_bench.Analysis_result.t) ->
          let open Core_bench.Analysis_result in
          print_endline (name ar);
          Array.to_list (regressions ar) |> List.filter ~f:(fun r ->
              let s = Variable.to_string @@ Regression.responder r in
              match s with "Time" -> true | _ -> false)
          |> List.map ~f:(fun r ->
              match Variable.to_string @@ Regression.responder r with
              | "Time" ->
                let coef = Regression.coefficients r in
                let sum = Array.to_list coef
                          |> List.fold_left ~init:0.0 ~f:(fun acc x -> acc +. (Coefficient.estimate x) ) in
                Float.to_string (sum /. (Float.of_int @@ Array.length coef))
              | _ -> failwith "some unsuported cases"
            )
          |> (function [x] -> put_result ~name:(name ar) x | _ -> failwith "Pattern matcing should be exhaustive");
        ) results;

      close_results ()

    )
