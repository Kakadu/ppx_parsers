open Printf
open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

let getenv s = try Sys.getenv s with Not_found -> ""

(* let (_: int) = default_mapper *)
let () = print_endline "ASDT"

let getenv_mapper argv =
  (* Our getenv_mapper only overrides the handling of expressions in the default mapper. *)
  { default_mapper with
    module_binding = fun mapper mb ->
      match mb with
      (* Is this an extension node? *)
      | { pmb_attributes = [ ({ txt = "parsers"; loc }, _) as attr ] }  ->
         printf "Good module is found!\n%!";
         let attr2 = ({ (fst attr) with txt="parsers2" }, snd attr) in
         default_mapper.module_binding mapper { mb with pmb_attributes = [attr2] }
        (* begin match pstr with *)
        (* | (\* Should have a single structure item, which is evaluation of a constant string. *\) *)
        (*   PStr [{ pstr_desc = *)
        (*           Pstr_eval ({ pexp_loc  = loc; *)
        (*                        pexp_desc = Pexp_constant (Const_string (sym, None))}, _)}] -> *)
        (*   (\* Replace with a constant string with the value from the environment. *\) *)
        (*   Exp.constant ~loc (Const_string (getenv sym, None)) *)
        (* | _ -> *)
        (*   raise (Location.Error ( *)
        (*           Location.error ~loc "[%getenv] accepts a string, e.g. [%getenv \"USER\"]")) *)
        (* end *)
      (* Delegate to the default mapper. *)
      | x -> default_mapper.module_binding mapper x
  }

let () = register "getenv" getenv_mapper
