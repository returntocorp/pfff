(* Yoann Padioleau
 *
 * Copyright (C) 2020 r2c
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
open Common
open Parser_ruby

(*****************************************************************************)
(* Token Helpers *)
(*****************************************************************************)
let is_eof = function
  | T_EOF _ -> true
  | _ -> false

let is_comment = function
  | _ -> raise Todo

(*****************************************************************************)
(* Visitors *)
(*****************************************************************************)

let visitor_info_of_tok f = function
  | T_EOF ii -> T_EOF (f ii)
  | T_EOL ii -> T_EOL (f ii)

  | T_UAMPER ii -> T_UAMPER (f ii)
  | T_AMPER ii -> T_AMPER (f ii)
  | T_TILDE ii -> T_TILDE (f ii)
  | T_BANG ii -> T_BANG (f ii)
  | T_VBAR ii -> T_VBAR (f ii)
  | T_CARROT ii -> T_CARROT (f ii)
  | T_PERCENT ii -> T_PERCENT (f ii)
  | T_SLASH ii -> T_SLASH (f ii)
  | T_USTAR ii -> T_USTAR (f ii)
  | T_STAR ii -> T_STAR (f ii)
  | T_RSHFT ii -> T_RSHFT (f ii)
  | T_LSHFT ii -> T_LSHFT (f ii)
  | T_DOT3 ii -> T_DOT3 (f ii)
  | T_DOT2 ii -> T_DOT2 (f ii)
  | T_NMATCH ii -> T_NMATCH (f ii)
  | T_MATCH ii -> T_MATCH (f ii)
  | T_OROP ii -> T_OROP (f ii)
  | T_ANDOP ii -> T_ANDOP (f ii)
  | T_GT ii -> T_GT (f ii)
  | T_LT ii -> T_LT (f ii)
  | T_LEQ ii -> T_LEQ (f ii)
  | T_GEQ ii -> T_GEQ (f ii)
  | T_NEQ ii -> T_NEQ (f ii)
  | T_EQQ ii -> T_EQQ (f ii)
  | T_EQ ii -> T_EQ (f ii)
  | T_ASSIGN ii -> T_ASSIGN (f ii)
  | T_CMP ii -> T_CMP (f ii)
  | T_POW ii -> T_POW (f ii)
  | T_UMINUS ii -> T_UMINUS (f ii)
  | T_MINUS ii -> T_MINUS (f ii)
  | T_UPLUS ii -> T_UPLUS (f ii)
  | T_PLUS ii -> T_PLUS (f ii)
  | T_USCOPE ii -> T_USCOPE (f ii)
  | T_SCOPE ii -> T_SCOPE (f ii)
  | T_SEMICOLON ii -> T_SEMICOLON (f ii)
  | T_COLON ii -> T_COLON (f ii)
  | T_QUESTION ii -> T_QUESTION (f ii)
  | T_RBRACE ii -> T_RBRACE (f ii)
  | T_LBRACE_ARG ii -> T_LBRACE_ARG (f ii)
  | T_LBRACE ii -> T_LBRACE (f ii)
  | T_RBRACK ii -> T_RBRACK (f ii)
  | T_LBRACK ii -> T_LBRACK (f ii)
  | T_LBRACK_ARG ii -> T_LBRACK_ARG (f ii)
  | T_RPAREN ii -> T_RPAREN (f ii)
  | T_LPAREN_ARG ii -> T_LPAREN_ARG (f ii)
  | T_LPAREN ii -> T_LPAREN (f ii)
  | T_ASSOC ii -> T_ASSOC (f ii)
  | T_COMMA ii -> T_COMMA (f ii)
  | T_DOT ii -> T_DOT (f ii)

  | K_FALSE ii -> K_FALSE (f ii)
  | K_TRUE ii -> K_TRUE (f ii)
  | K_SELF ii -> K_SELF (f ii)
  | K_YIELD ii -> K_YIELD (f ii)
  | K_NIL ii -> K_NIL (f ii)
  | K_lEND ii -> K_lEND (f ii)
  | K_lBEGIN ii -> K_lBEGIN (f ii)
  | K_NOT ii -> K_NOT (f ii)
  | K_OR ii -> K_OR (f ii)
  | K_AND ii -> K_AND (f ii)
  | K_RETURN ii -> K_RETURN (f ii)
  | K_DO ii -> K_DO (f ii)
  | K_IN ii -> K_IN (f ii)
  | K_FOR ii -> K_FOR (f ii)
  | K_UNTIL ii -> K_UNTIL (f ii)
  | K_WHILE ii -> K_WHILE (f ii)
  | K_WHEN ii -> K_WHEN (f ii)
  | K_CASE ii -> K_CASE (f ii)
  | K_ELSE ii -> K_ELSE (f ii)
  | K_ELSIF ii -> K_ELSIF (f ii)
  | K_THEN ii -> K_THEN (f ii)
  | K_UNLESS ii -> K_UNLESS (f ii)
  | K_IF ii -> K_IF (f ii)
  | K_ENSURE ii -> K_ENSURE (f ii)
  | K_RESCUE ii -> K_RESCUE (f ii)
  | K_BEGIN ii -> K_BEGIN (f ii)
  | K_UNDEF ii -> K_UNDEF (f ii)
  | K_ALIAS ii -> K_ALIAS (f ii)
  | K_END ii -> K_END (f ii)
  | K_DEF ii -> K_DEF (f ii)
  | K_MODULE ii -> K_MODULE (f ii)
  | K_CLASS ii -> K_CLASS (f ii)

  | T_ATOM_BEG ii -> T_ATOM_BEG (f ii)
  | T_REGEXP_BEG ii -> T_REGEXP_BEG (f ii)
  | T_TICK_BEG ii -> T_TICK_BEG (f ii)
  | T_DOUBLE_BEG ii -> T_DOUBLE_BEG (f ii)

  | T_OP_ASGN (s, ii) -> T_OP_ASGN (s, f ii)
  | T_INTERP_END (s, ii) -> T_INTERP_END (s, f ii)
  | T_INTERP_STR (s, ii) -> T_INTERP_STR (s, f ii)
  | T_REGEXP_MOD (s, ii) -> T_REGEXP_MOD (s, f ii)
  | T_USER_BEG (s, ii) -> T_USER_BEG (s, f ii)
  | T_SINGLE_STRING (s, ii) -> T_SINGLE_STRING (s, f ii)
  | T_BUILTIN_VAR (s, ii) -> T_BUILTIN_VAR (s, f ii)
  | T_CLASS_VAR (s, ii) -> T_CLASS_VAR (s, f ii)
  | T_INST_VAR (s, ii) -> T_INST_VAR (s, f ii)
  | T_GLOBAL_VAR (s, ii) -> T_GLOBAL_VAR (s, f ii)
  | T_LID (s, ii) -> T_LID (s, f ii)
  | T_UID (s, ii) -> T_UID (s, f ii)

  | T_REGEXP (a,b, ii) -> T_REGEXP (a,b, f ii)
  | T_USER_STRING (a,b, ii) -> T_USER_STRING (a,b, f ii)
  | T_DOUBLE_STRING (s, ii) -> T_DOUBLE_STRING (s, f ii)
  | T_FLOAT (a,b, ii) -> T_FLOAT (a,b, f ii)
  | T_BIGNUM (s, ii) -> T_BIGNUM (s, f ii)
  | T_FIXNUM (s, ii) -> T_FIXNUM (s, f ii)
  | T_ATOM (s, ii) -> T_ATOM (s, f ii)


let info_of_tok tok = 
  let res = ref None in
  visitor_info_of_tok (fun ii -> res := Some ii; ii) tok |> ignore;
  match !res with
  | Some x -> x
  | None -> Parse_info.fake_info "NOTOK"
