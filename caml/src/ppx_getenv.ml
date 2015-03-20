open Printf
open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

module Case = struct
  let mk ?guard pc_lhs pc_rhs = { pc_guard=guard; pc_lhs; pc_rhs }
end

let log fmt = kprintf (printf ">>> %s\n%!") fmt

type past =
  | OExpr of Parsetree.expression
  | Look of string
  | Alt of past * past
  | AltList of past list

let rec parse_past root =
  let rec helper : Parsetree.expression -> past = fun root ->
    match root.pexp_desc with
    | Pexp_apply ({pexp_desc=Pexp_ident { txt=Lident "<|>"; _ }; _}, [(_,l); (_,r)]) ->
       Alt (helper l, helper r)
    | Pexp_apply ({pexp_desc=Pexp_ident { txt=Lident "look"; _ }; _},
                  [_, {pexp_desc=Pexp_constant (Const_string (patt,_)); _}]) ->
       log "Look with patt = '%s' found." patt;
       Look patt
    | _ -> OExpr root
  in helper root

let getenv s = try Sys.getenv s with Not_found -> ""

let is_a_parser attrs = List.fold_left (fun acc -> function ({txt="parser"; _},_) -> true | _ -> acc)  false attrs
let remove_parser_attr = List.filter (function ({txt="parser"; _},_) -> false | _ -> true)

let () = log "PPX_PARSERS"

let is_good_value_binding vb =
  (is_a_parser vb.pvb_attributes)
  && (match vb.pvb_pat.ppat_desc with
      | Ppat_var _ -> true
      | _ -> false)
(*
let classify_oper = function
  | Pexp_apply ({pexp_desc=Pexp_ident { txt=Lident "<|>"; _ }; _}, [(_,l); (_,r)]) ->
     log "<|> is found";
     `Alt (l,r)
  | _ -> `Unknown

let map_expr_body mapper expr =
  (* most magic should be here *)
  let open Ast_convenience in
  match classify_oper expr.pexp_desc with
  | `Unknown   -> default_mapper.expr mapper expr
  | `Alt (l,r) -> begin
      (* parser for alternatives *)
      log "(* parser for alternatives *)";
      let match_e = app l [evar "s"] in
      let cases =
        [ Pat.alias (Pat.construct (lid "Parsed") (Some (Pat.any ()))) (Location.mkloc "x" !default_loc), evar "x"
        ; pconstr "Failed" [Pat.any ()], app r [evar "s"]
        ] |> List.map (fun (pc_lhs, pc_rhs) -> {pc_guard=None; pc_lhs; pc_rhs})
      in
      let ans = Ast_helper.Exp.(fun_ "" None (pvar "s") (match_ match_e cases)) in
      ans
    end *)

let map_past (past: past) : Parsetree.expression =
  log "map_past";
  let open Ast_helper in
  let open Ast_convenience in
  let decl_ref ~name valexpr cont =
    Exp.(let_ Nonrecursive
              [Vb.mk (Pat.var @@ Location.mknoloc name) (apply (ident @@ lid "ref") [("", valexpr)]) ]
              cont)
  in

  let rec helper ansname = function
    | Look str  ->
       let match_expr = Exp.(apply (ident @@ lid "Lexer.look") [("", evar "_stream"); ("", Ast_convenience.str str)]) in
       let cases =
         [ Case.mk [%pat? (Some stream2)] [%expr ans := (stream2, [%e Exp.constant@@ Const_string (str,None)]) ]
         ; Case.mk [%pat? None] [%expr error := Printf.sprintf "can't parse '%s'" [%e Exp.constant@@Const_string (str,None)]]
         ]
       in
       Exp.(match_ match_expr cases)
       (* Exp.ident (Ast_convenience.lid "str") *)
    | Alt (l,r) -> [%expr let () = [%e helper ansname l] in if !error<>"" then [%e helper ansname r] ]
    | OExpr e -> e
    | _ -> Exp.ident (Ast_convenience.lid "yayaya")
  in

  let decl_fun cont = Exp.(fun_ "" None (pvar "_stream") cont) in
  let decl_error = decl_ref ~name:"error" @@ Exp.constant (Const_string ("", None)) in
(*  let decl_ans cont =
    Exp.let_ Nonrecursive [Vb.mk (Pat.var @@ Location.mknoloc "ans") (helper past)] cont
  in
 *)
  let decl_ans = decl_ref ~name:"ans" @@
                   [%expr (Lexer.create "", Obj.magic ())]
                   (* Exp.(apply (ident@@lid "Obj.magic") ["", construct (lid "()") None]) *)
  in
  let call_helper = Exp.(let_ Nonrecursive [Vb.mk (Pat.var @@Location.mknoloc "()") (helper "ans" past) ]) in
  let decl_unreferror =
    Exp.(let_ Nonrecursive [ Vb.mk (Pat.var @@ Location.mknoloc "error")
                                   (apply (ident@@lid "!") ["", ident@@lid "error"])
                           ])
  in

  decl_fun @@ decl_error @@ decl_ans @@ call_helper @@ decl_unreferror @@
  [%expr if error = "" then Parsed (snd !ans, (), fst !ans) else Failed ()]



let map_value_binding mapper (vb: value_binding) =
  assert (is_good_value_binding vb);
  let name =
    match vb.pvb_pat.ppat_desc with
    | Ppat_var ({txt; _}) -> txt
    | _ -> assert false
  in
  log "Found a value binding '%s'" name;
  let newbody = map_past (parse_past vb.pvb_expr) in
  { {vb with pvb_expr = newbody } with pvb_attributes=remove_parser_attr vb.pvb_attributes }
  (* { {vb with pvb_expr = map_expr_body mapper vb.pvb_expr } with pvb_attributes=remove_parser_attr vb.pvb_attributes } *)


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

let () = register "getenv" struct_item_mapper

(*
let rec has_attr name: Parsetree.attributes -> bool = function
  | [] -> false
  | ({txt;loc},_) :: _ when txt = name -> true
  | _ :: xs -> has_attr name xs

let remove_parsers_attr = List.filter (fun ({txt;_},_) -> txt<>"parsers")

let module_duplicate_mapper argv =
  { default_mapper with structure =
    fun mapper items ->
      let rec iter items =
        match items with
        | { pstr_desc=Pstr_module mb; pstr_loc } :: rest when has_attr "parsers" mb.pmb_attributes ->
           let old_ = {mb with pmb_attributes = remove_parsers_attr mb.pmb_attributes } in
           let new_ = {old_ with pmb_name = Location.mknoloc (old_.pmb_name.txt^"_orig") } in
           let old_ = {pstr_desc=Pstr_module old_; pstr_loc} in
           let old_ = (struct_item_mapper Sys.argv).structure_item mapper old_ in
           old_ :: {pstr_desc=Pstr_module new_; pstr_loc} :: (iter rest)
        | { pstr_loc } as item :: rest ->
           mapper.structure_item mapper item :: iter rest
        | [] -> []
      in
      iter items
  }

let () = register "module_duplicate" module_duplicate_mapper
 *)
