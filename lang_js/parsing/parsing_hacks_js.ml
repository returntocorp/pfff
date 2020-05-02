(* Yoann Padioleau
 *
 * Copyright (C) 2010, 2013 Facebook
 * Copyright (C) 2019 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 * 
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)

module Flag = Flag_parsing
module PI = Parse_info

module Ast = Cst_js
module T = Parser_js
module TH   = Token_helpers_js
module F = Ast_fuzzy

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The goal for this module is to retag tokens 
 * (e.g., a T_LPAREN in T_LPAREN_ARROW)
 * or insert tokens (e.g., T_VIRTUAL_SEMICOLON) to
 * help the grammar remains simple and unambiguous. See 
 * lang_cpp/parsing/parsing_hacks.ml for more information about
 * this technique.
 *
 * This module inserts fake virtual semicolons, which is known as
 * Automatic Semicolon Insertion, or ASI for short.
 * Those semicolons can be ommitted by the user (but really should not).
 * ASI works in two steps:
 *  - certain tokens can not be followed by a newline (e.g., continue)
 *    and we detect those tokens in this file.
 *  - we also insert semicolons during error recovery in parser_js.ml. After
 *    all that was what the spec says.
 * Note that we need both techniques. See parse_js.ml comment for 
 * the limitations of using just the second technique.
 *  
 * reference:
 *  -http://www.bradoncode.com/blog/2015/08/26/javascript-semi-colon-insertion
 *  -http://www.ecma-international.org/ecma-262/6.0/index.html#sec-automatic-semicolon-insertion
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* obsolete *)
let is_toplevel_keyword = function
 | T.T_IMPORT _ | T.T_EXPORT _ 
 | T.T_VAR _ | T.T_LET _ | T.T_CONST _
 | T.T_FUNCTION _
 -> true
 | _ -> false

(* obsolete *)
let rparens_of_if toks = 
  let toks = Common.exclude TH.is_comment toks in

  let stack = ref [] in

  let rparens_if = ref [] in

  toks |> Common2.iter_with_previous_opt (fun prev x -> 
    (match x with
    | T.T_LPAREN _ -> 
        Common.push prev stack;
    | T.T_RPAREN info ->
        if !stack <> [] then begin
        let top = Common2.pop2 stack in
        (match top with
        | Some (T.T_IF _) -> 
            Common.push info rparens_if
        | _ ->
            ()
        )
        end
    | _ -> ()
    )
  );
  !rparens_if

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* retagging:
 *  - '(' when part of an arrow expression
 *  - less: '<' when part of a polymorphic type (aka generic)
 *  - less: { when part of a pattern before an assignment
 *)
let fix_tokens toks = 
 try 
  let trees = Lib_ast_fuzzy.mk_trees { Lib_ast_fuzzy.
     tokf = TH.info_of_tok;
     kind = TH.token_kind_of_tok;
  } toks 
  in
  let retag_lparen = Hashtbl.create 101 in
  let retag_keywords = Hashtbl.create 101 in

  (* visit and tag *)
  let visitor = Lib_ast_fuzzy.mk_visitor { Lib_ast_fuzzy.default_visitor with
    Lib_ast_fuzzy.ktrees = (fun (k, _) xs ->
      (match xs with
      | F.Parens (i1, _, _)::F.Tok ("=>",_)::_res ->
          Hashtbl.add retag_lparen i1 true
      (* TODO: also handle typed arrows! *)
      | F.Tok("import", i1)::F.Parens _::_res ->
          Hashtbl.add retag_keywords i1 true
      | _ -> ()
      );
      k xs
    )
  }
  in
  visitor trees;

  (* use the tagged information and transform tokens *)
  toks |> List.map (function
    | T.T_LPAREN info when Hashtbl.mem retag_lparen info ->
      T.T_LPAREN_ARROW (info)
    | T.T_IMPORT info when Hashtbl.mem retag_keywords info ->
      T.T_ID (PI.str_of_info info, info)
    | x -> x
  )

  with Lib_ast_fuzzy.Unclosed (msg, info) ->
   if !Flag.error_recovery
   then toks
   else raise (Parse_info.Lexical_error (msg, info))


(*****************************************************************************)
(* ASI (Automatic Semicolon Insertion) part 1 *)
(*****************************************************************************)

let fix_tokens_ASI xs =

  let res = ref [] in
  let rec aux prev f xs = 
    match xs with
    | [] -> ()
    | e::l ->
        if TH.is_comment e
        then begin 
          Common.push e res;
          aux prev f l
        end else begin
          f prev e;
          aux e f l
        end
  in

  let push_sc_before_x x = 
     let fake = Ast.fakeInfoAttach (TH.info_of_tok x) in
     Common.push (T.T_VIRTUAL_SEMICOLON fake) res; 
  in

  let f = (fun prev x ->
     (match prev, x with
     | (T.T_CONTINUE _ | T.T_BREAK _), _
        when TH.line_of_tok x <> TH.line_of_tok prev ->
        push_sc_before_x x;
     (* very conservative; should be any last(left_hand_side_expression) 
      * but for that better to rely on ASI via parse-error recovery;
      * no ambiguity like for continue because 
      *    if(true) x
      *    ++y;
      * is not valid.
      *)
     | (T.T_ID _ | T.T_FALSE _ | T.T_TRUE _), (T.T_INCR _ | T.T_DECR _)
        when TH.line_of_tok x <> TH.line_of_tok prev ->
        push_sc_before_x x;
     | _ -> ()
     );
     Common.push x res;
  ) in

  (* obsolete *)
  let rparens_if = rparens_of_if xs in
  let hrparens_if = Common.hashset_of_list rparens_if in

  (* history: this had too many false positives, which forced
   * to rewrite the grammar to add extra virtual semicolons which
   * then make the whole thing worse
   *)
  let _fobsolete = (fun prev x ->
    match prev, x with
    (* { } or ; } TODO: source of many issues *)
    | (T.T_LCURLY _ | T.T_SEMICOLON _), 
      T.T_RCURLY _ ->
        Common.push x res;
    (* <not } or ;> } *)
    | _, 
      T.T_RCURLY _ ->
        push_sc_before_x x;
        Common.push x res;
        
    (* ; EOF *)
    | (T.T_SEMICOLON _),
       T.EOF _ ->
        Common.push x res;
    (* <not ;> EOF *)
    | _, T.EOF _ ->
        push_sc_before_x x;
        Common.push x res;

    (* } 
     * <keyword>
     *)
    | T.T_RCURLY _, 
      (T.T_ID _
       | T.T_IF _ | T.T_SWITCH _ | T.T_FOR _
       | T.T_VAR _  | T.T_FUNCTION _ | T.T_LET _ | T.T_CONST _
       | T.T_RETURN _
       | T.T_BREAK _ | T.T_CONTINUE _
       (* todo: sure? *)
       | T.T_THIS _ | T.T_NEW _
      ) when TH.line_of_tok x <> TH.line_of_tok prev ->
        push_sc_before_x x;
        Common.push x res

    (* )
     * <keyword>
     *)
    (* this is valid only if the RPAREN is not the closing paren of an if*)
    | T.T_RPAREN info, 
      (T.T_VAR _ | T.T_IF _ | T.T_THIS _ | T.T_FOR _ | T.T_RETURN _ |
       T.T_ID _ | T.T_CONTINUE _ 
      ) when TH.line_of_tok x <> TH.line_of_tok prev 
             && not (Hashtbl.mem hrparens_if info) ->
        push_sc_before_x x;
        Common.push x res;


    (* ]
     * <keyword> 
     *)
    | T.T_RBRACKET _, 
      (T.T_FOR _ | T.T_IF _ | T.T_VAR _ | T.T_ID _)
      when TH.line_of_tok x <> TH.line_of_tok prev ->
        push_sc_before_x x;
        Common.push x res;

    (* <literal> 
     * <keyword> 
     *)
    | (T.T_ID _ 
        | T.T_NULL _ | T.T_STRING _ | T.T_REGEX _
        | T.T_FALSE _ | T.T_TRUE _
      ), 
       (T.T_VAR _ | T.T_ID _ | T.T_IF _ | T.T_THIS _ |
        T.T_RETURN _ | T.T_BREAK _ | T.T_ELSE _
      ) when TH.line_of_tok x <> TH.line_of_tok prev ->
        push_sc_before_x x;
        Common.push x res;

    (* } or ; or , or =
     * <keyword> col 0
     *)
    | (T.T_RCURLY _ | T.T_SEMICOLON _ | T.T_COMMA _ | T.T_ASSIGN _),
      _ 
      when is_toplevel_keyword x &&
       TH.line_of_tok x <> TH.line_of_tok prev && TH.col_of_tok x = 0
      ->
       Common.push x res;

    (* <no ; or }>
     * <keyword> col 0
     *)
    | _, _
      when is_toplevel_keyword x &&
       TH.line_of_tok x <> TH.line_of_tok prev && TH.col_of_tok x = 0
      ->
       push_sc_before_x x;
       Common.push x res;


    (* else *)
    | _, _ ->        
        Common.push x res;
  )
  in
  match xs with
  | [] -> []
  | x::_ ->
      let sentinel = 
        let fake = Ast.fakeInfoAttach (TH.info_of_tok x) in
        (T.T_SEMICOLON fake)
      in
      aux sentinel f xs;
      List.rev !res
