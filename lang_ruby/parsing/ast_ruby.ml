(* Mike Furr
 *
 * Copyright (C) 2010 Mike Furr
 * Copyright (C) 2020 r2c
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of the <organization> nor the
 *       names of its contributors may be used to endorse or promote products
 *       derived from this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Abstract Syntax Tree for Ruby 1.9
 *
 * Most of the code in this file derives from code from 
 * Mike Furr in diamondback-ruby.
 *
 * todo: 
 *  - [AST Format of the Whitequark parser](https://github.com/whitequark/parser/blob/master/doc/AST_FORMAT.md)
 *  - https://rubygems.org/gems/ast
 *)

(*****************************************************************************)
(* Names *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Token/info *)
(* ------------------------------------------------------------------------- *)
(* TODO: type tok = Parse_info.t and wrap and bracket *)
type tok = Lexing.position
 (* with tarzan *)

(*****************************************************************************)
(* Expression *)
(*****************************************************************************)

type expr = 
  | Literal of lit_kind * tok

  | Id of id_kind * string * tok
  | Operator of binary_op * tok
  | UOperator of unary_op * tok

  | Hash of bool * expr list * tok
  | Array of expr list * tok
  | Tuple of expr list * tok

  | Unary of unary_op * expr * tok
  | Binop of expr * binary_op * expr * tok
  | Ternary of expr * expr * expr * tok

  | Call of expr * expr list * expr option * tok

  | CodeBlock of bool * formal_param list option * expr list * tok

(*****************************************************************************)
(* Statement *)
(*****************************************************************************)

  | Empty
  | Block of expr list * tok

  | If of expr * expr list * expr list * tok
  | While of bool * expr * expr list * tok
  | Until of bool * expr * expr list * tok
  | Unless of expr * expr list * expr list * tok
  | For of formal_param list * expr * expr list * tok

  | Return of expr list * tok
  | Yield of expr list * tok

  | Case of case_block * tok

  | ExnBlock of body_exn * tok

(*****************************************************************************)
(* Definitions *)
(*****************************************************************************)

  | ClassDef of expr * inheritance_kind option * body_exn * tok
  | MethodDef of expr * formal_param list * body_exn * tok
  | ModuleDef of expr * body_exn * tok

  | BeginBlock of expr list * tok
  | EndBlock of expr list * tok

  | Alias of expr * expr * tok
  | Undef of expr list * tok

(*****************************************************************************)
(* Type *)
(*****************************************************************************)
  | Annotate of expr * Annotation.t * tok

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)

and lit_kind = 
  | FixNum of int
  | BigNum of Big_int.big_int
  | Float of string * float
  | String of string_kind
  | Atom of interp_string
  | Regexp of interp_string * string
  | Nil
  | Self
  | True
  | False

  and string_kind = 
    | Single of string
    | Double of interp_string
    | Tick of interp_string

  and interp_string = string_contents list

  and string_contents = 
    | StrChars of string
    | StrExpr of expr

and id_kind = 
  | ID_Lowercase (* prefixed by [a-z] or _ *)
  | ID_Instance  (* prefixed by @ *)
  | ID_Class     (* prefixed by @@ *)
  | ID_Global    (* prefixed by $ *)
  | ID_Uppercase (* prefixed by [A-Z] *)
  | ID_Builtin   (* prefixed by $, followed by non-alpha *)
  | ID_Assign of id_kind (* postfixed by = *)

and unary_op = 
  | Op_UMinus    (* -x *)  | Op_UPlus     (* +x *)
  | Op_UBang     (* !x *)
  | Op_UTilde    (* ~x *)
  | Op_UNot      (* not x *)
(*  | Op_UDefined*)  (* defined? x *)
  | Op_UAmper    (* & *)
  | Op_UStar     (* * *)
  | Op_UScope    (* ::x *)


and binary_op = 
  | Op_ASSIGN   (* = *)
  | Op_PLUS     (* + *)  | Op_MINUS    (* - *)
  | Op_TIMES    (* * *)  | Op_REM      (* % *)  | Op_DIV      (* / *)
  | Op_CMP  	(* <=> *)
  | Op_EQ  	(* == *)  | Op_EQQ  	(* === *)
  | Op_NEQ  	(* != *)
  | Op_GEQ  	(* >= *)  | Op_LEQ  	(* <= *)
  | Op_LT       (* < *)  | Op_GT       (* > *)
  | Op_AND      (* && *)  | Op_OR	(* || *)
  | Op_BAND     (* & *)  | Op_BOR      (* | *)
  | Op_MATCH    (* =~ *)
  | Op_NMATCH   (* !~ *)
  | Op_XOR      (* ^ *)
  | Op_POW      (* ** *)
  | Op_kAND     (* and *)  | Op_kOR      (* or *)
  | Op_ASSOC    (* => *)

  | Op_DOT      (* . *)

  | Op_SCOPE    (* :: *)
  | Op_AREF     (* [] *)
  | Op_ASET     (* []= *)
  | Op_LSHIFT   (* < < *)  | Op_RSHIFT   (* > > *)
(*  | Op_Custom of string*)   (* ?? *)
  | Op_OP_ASGN of binary_op  (* +=, -=, ... *)
  | Op_DOT2     (* .. *)
  | Op_DOT3     (* ... *)

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)

and formal_param = 
  | Formal_id of expr
  | Formal_amp of string
  | Formal_star of string (* as in *x *)
  | Formal_rest (* just '*' *)
  | Formal_tuple of formal_param list
  | Formal_default of string * expr

and inheritance_kind = 
  | Class_Inherit of expr
  | Inst_Inherit of expr

and body_exn = {
  body_exprs: expr list;
  rescue_exprs: (expr * expr) list;
  ensure_expr: expr list;
  else_expr: expr list;
}

and case_block = {
  case_guard : expr;
  case_whens: (expr list * expr list) list;
  case_else: expr list;
}

 (* with tarzan *)

(*****************************************************************************)
(* Toplevel *)
(*****************************************************************************)

type ast = expr list
 (* with tarzan *)

type pos = tok
 (* with tarzan *)
