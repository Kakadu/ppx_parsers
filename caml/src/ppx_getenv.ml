(**
 *  Some experiments with parser-combinators
 *
 *  Example:
 *  module ASDF1  = struct
 *    include Comb.Make(Lexer.SimpleStream)
 *    module Lexer = Lexer.SimpleStream (* TODO: move it to functor *)
 *    let f = (look "true") [@@parser]
 *  end [@@parsers]
 *
 *  Syntax extension sees [@@parsers] and clones this module into two. 1st one has the same name and contains
 *  modified parser where parser function were a little but rewritten as recursive descent. 2nd one is renamed
 *  original parser with removed attributes inside the module.
 *)
open Printf
open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

module Case = struct
  let mk ?guard pc_lhs pc_rhs = { pc_guard=guard; pc_lhs; pc_rhs }
end

let log fmt = kprintf (printf "(* >>> %s *)\n%!") fmt

type past =
  | OExpr of Parsetree.expression
  | Look of string
  | Alt of past * past
  | Many of past
  | RepSep of past*past (* parse and separator *)
  | Whitespace
  | Left  of past*past
  | Right of past*past
  (* | AltList of past list *)
  | Map of past * Parsetree.expression

let rec parse_past root =
  let rec helper : Parsetree.expression -> past = fun root ->
    match root.pexp_desc with
    | Pexp_apply ({pexp_desc=Pexp_ident { txt=Lident "<|>"; _ }; _}, [(_,l); (_,r)]) ->
       Alt (helper l, helper r)
    | Pexp_apply ({pexp_desc=Pexp_ident { txt=Lident "look"; _ }; _},
                  [_, {pexp_desc=Pexp_constant (Const_string (patt,_)); _}]) ->
       (* log "Look with patt = '%s' found." patt; *)
       Look patt
    | Pexp_apply ({pexp_desc=Pexp_ident { txt=Lident    "-->"; _ }; _}, [(_,l); (_,r)]) -> Map   (helper l, r)
    | Pexp_apply ({pexp_desc=Pexp_ident { txt=Lident    "@~>"; _ }; _}, [(_,l); (_,r)]) -> Right (helper l, helper r)
    | Pexp_apply ({pexp_desc=Pexp_ident { txt=Lident    "<~@"; _ }; _}, [(_,l); (_,r)]) -> Left  (helper l, helper r)
    | Pexp_apply ({pexp_desc=Pexp_ident { txt=Lident "repsep"; _ }; _}, [(_,l); (_,r)]) -> RepSep (helper l, helper r)
    | Pexp_apply ({pexp_desc=Pexp_ident { txt=Lident "many"; _ }; _}, [(_,l)]) -> Many (helper l)
    | Pexp_apply ({pexp_desc=Pexp_ident { txt=Lident "ws"; _ }; _}, []) -> Whitespace
    | _ -> OExpr root
  in helper root

let rec has_attr name = List.fold_left (fun acc -> function ({txt; _},_) when txt=name -> true | _ -> acc)  false
let is_a_parser = has_attr "parser"
let remove_attr name = List.filter (fun ({txt;_},_) -> txt<>name)
let remove_parser_attr = remove_attr "parser"
let remove_parsers_attr = remove_attr "parsers"

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

(* Ast evaluation and unfolding parser-combinators into recursive descent is there *)
let map_past (past: past) : Parsetree.expression =
  let open Ast_helper in
  let open Ast_convenience in
  (* let decl_ref ~name valexpr cont = *)
  (*   Exp.(let_ Nonrecursive *)
  (*             [Vb.mk (Pat.var @@ Location.mknoloc name) (apply (ident @@ lid "ref") [("", valexpr)]) ] *)
  (*             cont) *)
  (* in *)

  let (make_var, make_vars) =
    let counter = ref 0 in
    ( (fun () -> incr counter; sprintf "_ans%d" !counter)
    , (fun () -> incr counter; (sprintf "_ans%d" !counter, sprintf "_stream%d" !counter) )
    )
  in
  let rec helper ans_name ans_stream = function
    | Look str   ->
      (* log "look for '%s'" str; *)
       (* We generate some code which skip names*)
       let temp_stream = make_var () in
       [%expr let [%p pvar temp_stream] = Lexer.skip_ws ! [%e evar ans_stream] in
              match Lexer.look [%e evar temp_stream] [%e Ast_convenience.str str] with
              | Some new_stream -> [%e evar ans_name]   := [%e Ast_convenience.str str];
                                   [%e evar ans_stream] := new_stream
              | None ->  error := Printf.sprintf "can't parse '%s'" [%e Exp.constant@@Const_string (str,None)]
       ]

    | Alt (l,r) ->
       let (_,temp_stream) = make_vars () in
       [%expr let [%p pvar temp_stream ] = [%e evar ans_stream] in
              let () = [%e helper ans_name temp_stream l] in
              if !error<>"" then begin
                  [%e evar temp_stream] := ! [%e evar ans_stream];
                  let () = [%e helper ans_name temp_stream r] in
                  if !error="" then begin
                      [%e evar ans_stream] := ! [%e evar temp_stream];
                    end
                end
       ]
    | Right (l_ast,r_ast) ->
      (* log "@~> is processed"; *)
      let (l_ans, temp_stream) = make_vars () in
      let r_ans = make_var () in
      [%expr let [%p pvar temp_stream] = [%e evar ans_stream] in
             let [%p pvar l_ans]       = [%e Obj.magic() ] in
             let () = [%e helper l_ans temp_stream l_ast] in
             if !error="" then begin
               let [%p pvar r_ans]       = [%e Obj.magic() ] in
               let () = [%e helper r_ans temp_stream r_ast] in
               ()
               (* (if !error="" then ([%e evar ans_stream] := ![%e evar temp_stream]; *)
               (*                     [%e evar ans_name  ] := ![%e evar r_ans])) *)
             end
      ]
    | Left  (l_ast,r_ast) ->
      (* log "<~@ is processed"; *)
      [%expr 1]
    | RepSep(l_ast,r_ast) ->
      [%expr 1]
        (* assert false *)
    | Map (l_ast,r_expr) ->
       (* log "map result is found!"; *)
       let temp_ans_name,temp_stream_name = make_vars () in
       [%expr let [%p pvar temp_ans_name ] = ref (Obj.magic()) in
              let [%p pvar temp_stream_name ] = [%e evar ans_stream] in
              let () = [%e helper temp_ans_name temp_stream_name l_ast] in
              if !error="" then begin
                  [%e evar ans_stream] := ! [%e evar temp_stream_name];
                  [%e evar ans_name]   := [%e r_expr] (! [%e evar temp_ans_name]);
                end]

    | Many past ->
       let temp_ans_name,temp_stream_name = make_vars () in
       let temp_ans_xs_name = make_var () in
       [%expr let [%p pvar temp_ans_name ] = ref (Obj.magic ()) in
              let [%p pvar temp_stream_name ] = ref (! [%e evar ans_stream]) in
              let [%p pvar temp_ans_xs_name ] = ref [] in
              let rec loop () =
                let () = [%e helper temp_ans_name temp_stream_name past] in
                if !error="" then begin
                    [%e evar temp_ans_xs_name] := ! [%e evar temp_ans_name] :: ! [%e evar temp_ans_xs_name];
                    loop ()
                  end else ( [%e evar ans_name] := List.rev ! [%e evar temp_ans_xs_name];
                             [%e evar ans_stream] := ! [%e evar temp_stream_name];
                             error := ""
                           )
              in
              loop ()
       ]
    | Whitespace -> [%expr failwith "not implemented" ]
    | OExpr e -> e
  in

  [%expr fun _stream ->
         let error = ref "" in
         let ans = ref (Obj.magic ()) in
         let cur_stream = ref _stream in
         let () = [%e (helper "ans" "cur_stream" past)] in
         if !error="" then Parsed(!ans, (), !cur_stream) else Failed ()
  ]


(* Just erase parser attributes from parser functions there there *)
let struct_item_parser_eraser _args : Ast_mapper.mapper =
  let map_value_binding mapper (vb: value_binding) =
    assert (is_good_value_binding vb);
    { vb with pvb_attributes=remove_parser_attr vb.pvb_attributes }
  in
  { default_mapper with
    structure_item = fun mapper sitem ->
      match sitem.pstr_desc with
      | Pstr_value (_rec,xs) ->
         let f vb = if is_good_value_binding vb then map_value_binding mapper vb else vb in
         { sitem with pstr_desc =  Pstr_value (_rec, List.map f xs) }
      | x -> default_mapper.structure_item mapper sitem
  }

(* map structure_item with parser generation *)
let struct_item_mapper args : Ast_mapper.mapper =
  let map_value_binding mapper (vb: value_binding) =
    assert (is_good_value_binding vb);
    (* let name = *)
    (*   match vb.pvb_pat.ppat_desc with *)
    (*   | Ppat_var ({txt; _}) -> txt *)
    (*   | _ -> assert false *)
    (* in *)
    (* log "Found a value binding '%s'" name; *)
    let newbody = map_past (parse_past vb.pvb_expr) in
    { {vb with pvb_expr = newbody } with pvb_attributes=remove_parser_attr vb.pvb_attributes }
  in
  { default_mapper with
    structure_item = fun mapper sitem ->
      match sitem.pstr_desc with
      | Pstr_value (_rec,xs) ->
         let f vb = if is_good_value_binding vb then map_value_binding mapper vb else vb in
         { sitem with pstr_desc =  Pstr_value (_rec, List.map f xs) }
      | x -> default_mapper.structure_item mapper sitem
  }

let module_duplicate_mapper argv =
  { default_mapper with structure =
    fun mapper items ->
      let rec iter items =
        match items with
        | { pstr_desc=Pstr_module mb; pstr_loc } :: rest when has_attr "parsers" mb.pmb_attributes ->
           let old_ = {mb with pmb_attributes = remove_parsers_attr mb.pmb_attributes } in
           let new_ =
             let new_ = {old_ with pmb_name = Location.mknoloc (old_.pmb_name.txt^"_orig") } in
             let new_ = {pstr_desc=Pstr_module new_; pstr_loc} in
             let useful_mapper = struct_item_parser_eraser [] in
             useful_mapper.structure_item useful_mapper new_
           in
           (* Invesigate parser functions now *)
           let old_ = {pstr_desc=Pstr_module old_; pstr_loc} in
           let useful_mapper = struct_item_mapper [] in
           let old_ = useful_mapper.structure_item useful_mapper old_ in
           old_ :: new_ :: (iter rest)
        | { pstr_loc } as item :: rest ->
           mapper.structure_item mapper item :: iter rest
        | [] -> []
      in
      iter items
  }

let () = register "module_duplicate" module_duplicate_mapper
