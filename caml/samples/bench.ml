open TstJSON
open Core_bench
open Core_bench.Std
open Core.Std
open Printf

let () =
  let put_result,close_results =
    let ch = open_out "results.rez" in
    let put ~name time = Printf.fprintf ch "%s,%s\n%!" name time in
    let close () = Out_channel.close ch in
    (put,close)
  in

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

  let meass = Bench.measure ~run_config:(Bench.Run_config.create ~time_quota:(Time.Span.of_sec 2.0) ()) small_tests in
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
            let sum = Array.to_list coef |> List.fold_left ~init:0.0 ~f:(fun acc x -> acc +. (Coefficient.estimate x) ) in
            Float.to_string (sum /. (Float.of_int @@ Array.length coef))
          | _ -> failwith "some unsuported cases"
        )
      (* |> String.concat ~sep:"," |> print_endline; *)
      |> (function [x] -> put_result ~name:(name ar) x | _ -> failwith "Pattern matcing should be exhaustive");
    ) results;

  close_results ()
