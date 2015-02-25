open Printf
open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

let getenv s = try Sys.getenv s with Not_found -> ""

let is_a_parser attrs = List.fold_left (fun acc -> function ({txt="parser"; _},_) -> true && acc | _ -> acc) false attrs
let log fmt = kprintf (printf ">>> %s\n%!") fmt

let () = log "PPX_PARSERS"

let is_good_value_binding vb =
  (is_a_parser vb.pvb_attributes)
  && (match vb.pvb_pat.ppat_desc with
      | Ppat_var _ -> true
      | _ -> false)


let map_value_binding mapper (vb: value_binding) =
  log "2";
  assert (is_good_value_binding vb);
  let name =
    match vb.pvb_pat.ppat_desc with
    | Ppat_var ({txt; _}) -> txt
    | _ -> assert false
  in
  log "Found a good function '%s'" name;
  default_mapper.value_binding mapper vb

let struct_item_mapper argv =
  log "struct_item_mapper ";
  { default_mapper with
    structure_item = fun mapper sitem ->
      match sitem.pstr_desc with
      | Pstr_value (_rec,xs) ->
         log "1";
         let f vb = if is_good_value_binding vb then map_value_binding mapper vb else vb in
         { sitem with pstr_desc =  Pstr_value (_rec, List.map f xs) }
      | x -> default_mapper.structure_item mapper sitem
  }

let getenv_mapper argv =
  (* Our getenv_mapper only overrides the handling of expressions in the default mapper. *)
  let ans = { default_mapper with
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
  in
  ans


let () = register "getenv" struct_item_mapper
