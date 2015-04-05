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
  | Decimal
  | StringLit
  | Alt of past * past
  | Many  of past   (* EBNF * *)
  | Many1 of past   (* EBNF + *)
  | RepSep  of past*past (* item and separator parsers *)
  | RepSep1 of past*past (* at least 1 item should be parsed *)
  | Whitespace
  | Left  of past*past
  | Right of past*past
  | Pair  of past*past     (* <@>: 'a parser -> 'b parser -> ('a * 'b) parser *)
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
    | Pexp_apply ({pexp_desc=Pexp_ident { txt=Lident    "<@>"; _ }; _}, [(_,l); (_,r)]) -> Pair  (helper l, helper r)
    | Pexp_apply ({pexp_desc=Pexp_ident { txt=Lident  "listBy"; _ }; _}, [(_,l); (_,r)]) -> RepSep (helper l, helper r)
    | Pexp_apply ({pexp_desc=Pexp_ident { txt=Lident "list1By"; _ }; _}, [(_,l); (_,r)]) -> RepSep1 (helper l, helper r)
    | Pexp_apply ({pexp_desc=Pexp_ident { txt=Lident  "many"; _ }; _}, [(_,l)]) -> Many (helper l)
    | Pexp_apply ({pexp_desc=Pexp_ident { txt=Lident "many1"; _ }; _}, [(_,l)]) -> Many1 (helper l)
    | Pexp_ident { txt=Lident      "decimal"; _ }                               -> Decimal
    | Pexp_ident { txt=Lident   "string_lit"; _ }                               -> StringLit
    | Pexp_ident { txt=Lident  "whitespaces"; _ }
    | Pexp_ident { txt=Lident          "wss"; _ }                                      -> Whitespace
    | _ -> log "OExpr is read"; OExpr root
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
    | OExpr oexpr ->
      [%expr match [%e oexpr]  ![%e evar ans_stream] with
             | Parsed (rez,_errs,new_stream) -> [%e evar ans_stream] := new_stream;
                                                error := "";
                                                [%e evar ans_name] := rez
             | Failed _ -> error:= Printf.sprintf "Failed when applying OExpr"
      ]
    | Decimal ->
      [%expr let (start_pos, str) as init_lexer = ! [%e evar ans_stream] in
             if Lexer.is_finished init_lexer then error:="stream is ended"
             else begin
               let str_len = String.length str in
               let (cur_pos,sign) = if str.[start_pos] = '-' then (ref (start_pos+1),-1) else (ref start_pos,1) in
               let do_skip () =
                 while !cur_pos < str_len && (let c = int_of_char str.[!cur_pos] in (48<=c)&&(c<=57)) do
                   incr cur_pos done
               in
               do_skip ();
               if !cur_pos<str_len && str.[!cur_pos]='.' then (incr cur_pos; do_skip () );
               if !cur_pos<str_len && (str.[!cur_pos]='e' || str.[!cur_pos]='E')  then (incr cur_pos; do_skip () );
               let float_str = String.sub str start_pos (!cur_pos - start_pos) in
               print_endline float_str;
               try [%e evar ans_name]   := float_of_string float_str;
                   [%e evar ans_stream] := (!cur_pos, str)
               with Failure _ -> error:= "Failure float_of_string"
             end
       ]
    | StringLit ->
      [%expr let (start_pos, str) as init_lexer = ! [%e evar ans_stream] in
             if Lexer.is_finished init_lexer then error := "Stream is finished"
             else if str.[start_pos] <> '"' then error := "<error>"
             else
               let str_len = String.length str in
               let rec loop is_esc pos =
                 if pos >= str_len then error := "<error>" else
                   match str.[pos] with
                   | '"' when is_esc -> loop false (pos+1)
                   | '"' -> [%e evar ans_name] := String.sub str (start_pos+1) (pos-start_pos-1);
                            [%e evar ans_stream] := (pos+1,str)
                   | '\\' when is_esc -> loop false (pos+1)
                   | '\\' -> loop true (pos+1)
                   | _ -> loop false (pos+1)
               in
               loop false (start_pos+1)

      ]
    | Look str   ->
       (* We generate some code which skip names*)
       (* TODO: remove this skipping of whitespace, we will write that explicilty *)
       let temp_stream = make_var () in
       [%expr let [%p pvar temp_stream] = Lexer.skip_ws ! [%e evar ans_stream] in
              match Lexer.look [%e evar temp_stream] [%e Ast_convenience.str str] with
              | Some new_stream -> [%e evar ans_name]   := [%e Ast_convenience.str str];
                                   [%e evar ans_stream] := new_stream
              | None ->  error := Printf.sprintf "can't parse '%s'" [%e Exp.constant@@Const_string (str,None)]
       ]

    | Whitespace ->
      [%expr [%e evar ans_name] := (); [%e evar ans_stream] := Lexer.skip_ws ! [%e evar ans_stream] ]

    | Alt (l,r) ->
       let (_,temp_stream) = make_vars () in
       [%expr let [%p pvar temp_stream ] = [%e evar ans_stream] in
              let () = [%e helper ans_name temp_stream l] in
              if !error<>"" then begin
                  [%e evar temp_stream] := ! [%e evar ans_stream];
                  error := "";
                  let () = [%e helper ans_name temp_stream r] in
                  if !error="" then begin
                      [%e evar ans_stream] := ! [%e evar temp_stream];
                    end
                end
       ]
    | Right (l_ast,r_ast) ->
      let (l_ans, temp_stream) = make_vars () in
      let r_ans = make_var () in
      [%expr let [%p pvar temp_stream] = [%e evar ans_stream] in
             let [%p pvar l_ans]       = ref (Obj.magic()) in
             let () = [%e helper l_ans temp_stream l_ast] in
             if !error="" then begin
               let [%p pvar r_ans]     = ref (Obj.magic()) in
               let () = [%e helper r_ans temp_stream r_ast] in
               (if !error="" then ([%e evar ans_stream] := ![%e evar temp_stream];
                                   [%e evar ans_name  ] := ![%e evar r_ans]))
             end
      ]
    | Left  (l_ast,r_ast) ->
      let (l_ans, temp_stream) = make_vars () in
      let r_ans = make_var () in
      [%expr let [%p pvar temp_stream] = [%e evar ans_stream] in
             let [%p pvar l_ans]       = ref (Obj.magic()) in
             let () = [%e helper l_ans temp_stream l_ast] in
             if !error="" then begin
               let [%p pvar r_ans]     = ref (Obj.magic()) in
               let () = [%e helper r_ans temp_stream r_ast] in
               (if !error="" then ([%e evar ans_stream] := ![%e evar temp_stream];
                                   [%e evar ans_name  ] := ![%e evar l_ans]))
             end
      ]
    | Pair (l_ast,r_ast) ->
      let (l_ans, temp_stream) = make_vars () in
      let r_ans = make_var () in
      [%expr let [%p pvar temp_stream] = [%e evar ans_stream] in
             let [%p pvar l_ans]       = ref (Obj.magic()) in
             let () = [%e helper l_ans temp_stream l_ast] in
             if !error="" then
               let [%p pvar r_ans]     = ref (Obj.magic()) in
               let () = [%e helper r_ans temp_stream r_ast] in
               if !error="" then begin
                 [%e evar ans_stream] := ![%e evar temp_stream];
                 [%e evar ans_name  ] := (![%e evar l_ans], ![%e evar r_ans] )

               end
      ]
    | RepSep1(p_ast,sep_ast) ->
      let (sep_ans,temp_stream) = make_vars () in
      let item_ans = make_var () in
      let item_list = make_var () ^ "xs" in
      [%expr let [%p pvar temp_stream] = [%e evar ans_stream] in
             let [%p pvar sep_ans]   = ref (Obj.magic()) in
             let [%p pvar item_ans]  = ref (Obj.magic()) in
             let [%p pvar item_list] = ref (Obj.magic()) in
             let () = [%e helper item_ans temp_stream p_ast] in
             if !error="" then begin
               [%e evar item_list] := [ ! [%e evar item_ans] ];
               let rec loop () =
                 let save_stream: _ ref = [%e evar temp_stream] in
                 let () = [%e helper sep_ans "save_stream" sep_ast] in
                 if !error="" then (
                   let () = [%e helper item_ans "save_stream" p_ast] in
                   if !error="" then ( [%e evar item_list] := ![%e evar item_ans] :: ![%e evar item_list];
                                       [%e evar temp_stream] := ![%e evar "save_stream"];
                                       loop ())
                   else ( (* can't parse `item` after parsing `sep` *)
                     [%e evar temp_stream] := ! save_stream
                   )
                 ) else ( (* can' parse separator *)
                   [%e evar temp_stream] := ! save_stream (* seems to be copy past of previous else branch *)
                 )
               in
               loop ();
               error := "";
               [%e evar ans_name] := List.rev ! [%e evar item_list]
             end
      ]
    | RepSep(p_ast,sep_ast) ->
      let (sep_ans,temp_stream) = make_vars () in
      let item_ans = make_var () in
      let item_list = make_var () ^ "xs" in
      [%expr let [%p pvar temp_stream] = [%e evar ans_stream] in
             let [%p pvar sep_ans]   = ref (Obj.magic()) in
             let [%p pvar item_ans]  = ref (Obj.magic()) in
             let [%p pvar item_list] = ref (Obj.magic()) in
             let () = [%e helper item_ans temp_stream p_ast] in
             if !error="" then begin
               [%e evar item_list] := [ ! [%e evar item_ans] ];
               let rec loop () =
                 let save_stream: _ ref = [%e evar temp_stream] in
                 let () = [%e helper sep_ans "save_stream" sep_ast] in
                 if !error="" then (
                   let () = [%e helper item_ans "save_stream" p_ast] in
                   if !error="" then ( [%e evar item_list] := ![%e evar item_ans] :: ![%e evar item_list];
                                       [%e evar temp_stream] := ![%e evar "save_stream"];
                                       loop ())
                   else ( (* can't parse `item` after parsing `sep` *)
                     [%e evar temp_stream] := ! save_stream
                   )
                 ) else ( (* can' parse separator *)
                   [%e evar temp_stream] := ! save_stream (* seems to be copy past of previous else branch *)
                 )
               in
               loop ();
               error := "";
               [%e evar ans_name] := List.rev ! [%e evar item_list]
             end else begin
               error:= "";
               [%e evar ans_name] := [];
             end
      ]
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
    | Many1 past ->
       let item_name,temp_stream_name = make_vars () in
       let item_list_name = make_var () in
       [%expr let [%p pvar item_name] = ref (Obj.magic()) in
              let [%p pvar temp_stream_name]: _ ref = [%e evar ans_stream] in
              let [%p pvar item_list_name] = ref [] in
              let () = [%e helper item_name temp_stream_name past] in
              if !error="" then begin
                [%e evar item_list_name] := ! [%e evar item_name] :: [];
                let rec loop () =
                  let save_stream = [%e evar temp_stream_name] in
                  let () = [%e helper item_name "save_stream" past] in
                  if !error="" then begin
                    [%e evar item_list_name] := ! [%e evar item_name] :: ![%e evar item_list_name];
                    [%e evar temp_stream_name]   := !save_stream;
                    loop ()
                  end else begin
                    error := "";
                    [%e evar ans_stream] := ![%e evar temp_stream_name];
                    [%e evar ans_name] := ! [%e evar item_list_name]
                  end
                in
                loop ()
              end
       ]
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

  let rec canonize_vb vb =
    (* sometimes we should write awkward code to make it compilable
     *   let rec main stream = ((look "{") @~> (theval) <~@ (look "}")) @@ stream
     *   and theval stream = (look "x") stream
     *
     * instead of:
     *   let rec main stream = ((look "{") @~> (theval) <~@ (look "}")) @@ stream
     *   and theval = (look "x")
     *
     *)
    match vb with
    (* TODO: implement case for applying using @@ *)
    | { pvb_pat ={ppat_desc=Ppat_var {txt=funname;_}; _}
      ; pvb_expr={pexp_desc=Pexp_fun (_,None, {ppat_desc=Ppat_var {txt=stream_var_name; _}; _},
                                      {pexp_desc=Pexp_apply(parser_expr,
                                                            [("",{pexp_desc=Pexp_ident{txt=Lident arg_name;_};_})])
                                      ; _
                                      }
                                     )
                   ; _
                  }
      ; _
      } when stream_var_name=arg_name ->
      { vb with pvb_expr = parser_expr }
    | { pvb_pat ={ppat_desc=Ppat_var {txt=funname;_}; _}
      ; pvb_expr={pexp_desc=Pexp_fun (_label1, None, ({ppat_desc=Ppat_var {txt=stream_var_name; _}; _} as _2) ,
                                      {pexp_desc=Pexp_constraint(e, _typ); _ }
                                     )
                 ;
                 } as _e1
      ; _
      } ->
      (* Also specifing funnction type introduces constraints in the parse tree *)
      canonize_vb { vb with pvb_expr={_e1 with pexp_desc=Pexp_fun(_label1,None, _2, e) } }
    | _ -> vb
  in


  { default_mapper with
    structure_item = fun mapper sitem ->
      match sitem.pstr_desc with
      | Pstr_value (_rec,xs) ->
         let f vb = if is_good_value_binding vb then map_value_binding mapper @@ canonize_vb vb else vb in
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
