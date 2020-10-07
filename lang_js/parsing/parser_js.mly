%{
(* Yoann Padioleau
 *
 * Copyright (C) 2010-2014 Facebook
 * Copyright (C) 2019-2020 r2c
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

open Ast_js
module G = AST_generic (* for operators, fake, and now also type_ *)

(*************************************************************************)
(* Prelude *)
(*************************************************************************)
(* This file contains a grammar for Javascript (ES6 and more), as well
 * as partial support for Typescript.
 *
 * reference:
 *  - https://en.wikipedia.org/wiki/JavaScript_syntax
 *  - http://www.ecma-international.org/publications/standards/Ecma-262.htm
 *  - https://github.com/Microsoft/TypeScript/blob/master/doc/spec.md#A
 * 
 * src: originally ocamlyacc-ified from Marcel Laverdet 'fbjs2' via Emacs
 * macros, itself extracted from the official ECMAscript specification at:
 * http://www.ecma-international.org/publications/standards/ecma-262.htm
 * back in the day (probably ES4 or ES3).
 * 
 * I have heavily extended the grammar to provide the first parser for Flow.
 * I have extended it also to deal with many new Javascript features
 * (see cst_js.ml top comment). 
 *
 * The grammar is close to the ECMA grammar but I've simplified a few things 
 * when I could:
 *  - less intermediate grammar rules for advanced features
 *    (they are inlined in the original grammar rule)
 *  - by using my retagging-tokens technique (see parsing_hacks_js.ml) 
 *    I could also get rid of some of the ugliness in the ECMA grammar 
 *    that has to deal with ambiguous constructs
 *    (they conflate together expressions and arrow parameters, object
 *    values and object matching, etc.). 
 *    Instead, in this grammar things are clearly separated.
 *  - I've used some macros to factorize rules, including some tricky
 *    macros to factorize expression rules.
 *)

(*************************************************************************)
(* Helpers *)
(*************************************************************************)
let fb = G.fake_bracket

(* ugly, but in a sgrep pattern, anonymous functions are parsed as a toplevel
 * function decl (because 'function_decl' accepts id_opt,
 * see its comment to see the reason)
 * This is why we intercept this case by returning instead an Expr pattern.
 *)
let fix_sgrep_module_item _x =
  raise Todo
(* TODO
  match x with
  | It (FunDecl ({ f_kind = F_func (_, None); _ } as decl)) ->
      Expr (Function decl)
  (* less: could check that sc is an ASI *)
  | It (St (ExprStmt (e, _sc))) -> Expr e
  | _ -> ModuleItem x
*)

let mk_Id id =
  Id (id, ref NotResolved)

let mk_Fun ?(id=None) props (_generics,(_,f_params,_),f_rettype) (lc,xs,rc) = 
  let f_attrs = props |> List.map attr in
  Fun ({ f_params; f_body = Block (lc, xs, rc); f_rettype; f_attrs }, id)

let mk_Class ?(props=[]) tok idopt _generics (c_extends, c_implements) c_body =
  let c_attrs = props |> List.map attr in
  Class ({c_kind = G.Class, tok; c_extends; c_implements; c_attrs; c_body}, 
         idopt)

let mk_Field ?(fld_type=None) ?(props=[]) fld_name eopt =
  let fld_attrs = props |> List.map attr in
  Field { fld_name; fld_attrs; fld_type; fld_body = eopt }

let add_modifiers _propsTODO fld = 
  fld

let mk_Encaps _ _ = 
  raise Todo
let mk_Super tok =
  IdSpecial (Super, tok)

let mk_pattern binding_pattern init_opt =
  match init_opt with
  | None -> binding_pattern
  | Some (t, e) -> Assign (binding_pattern, t, e)

(* Javascript has implicit returns for arrows with expression body *)
let mk_block_return e = 
  fb [Return (G.fake "return", Some e)]

let special spec tok xs = 
  Apply (IdSpecial (spec, tok), fb xs)

let bop op a b c = special (ArithOp op) b [a;c]
let uop op tok x = special op tok [x]

let seq (e1, t, e2) = special Seq t [e1; e2]

let mk_Assign (e1, (tok, opopt), e2) =
  match opopt with
  | None -> Assign (e1, tok, e2)
  (* less: should use intermediate? can unsugar like this? *)
  | Some op -> Assign (e1, tok, special (ArithOp op) tok [e1;e2])
  

%}
(*************************************************************************)
(* Tokens *)
(*************************************************************************)

%token <Parse_info.t> TUnknown  (* unrecognized token *)
%token <Parse_info.t> EOF

(*-----------------------------------------*)
(* The space/comment tokens *)
(*-----------------------------------------*)
(* coupling: Token_helpers.is_comment *)
%token <Parse_info.t> TCommentSpace TCommentNewline   TComment

(*-----------------------------------------*)
(* The normal tokens *)
(*-----------------------------------------*)

(* tokens with a value *)
%token<string * Parse_info.t> T_NUMBER
%token<string * Parse_info.t> T_ID

%token<string * Parse_info.t> T_STRING 
%token<string * Parse_info.t> T_ENCAPSED_STRING
%token<string * Parse_info.t> T_REGEX

(*-----------------------------------------*)
(* Keyword tokens *)
(*-----------------------------------------*)
(* coupling: if you add an element here, expand also ident_keyword_bis
 * and also maybe the special hack for regexp in lexer_js.mll *)
%token <Parse_info.t>
 T_FUNCTION T_CONST T_VAR T_LET
 T_IF T_ELSE
 T_WHILE T_FOR T_DO T_CONTINUE T_BREAK
 T_SWITCH T_CASE T_DEFAULT 
 T_RETURN 
 T_THROW T_TRY T_CATCH T_FINALLY
 T_YIELD T_ASYNC T_AWAIT
 T_NEW T_IN T_OF T_THIS T_SUPER T_WITH
 T_NULL T_FALSE T_TRUE
 T_CLASS T_INTERFACE T_EXTENDS T_IMPLEMENTS T_STATIC T_GET T_SET T_CONSTRUCTOR
 T_IMPORT T_EXPORT T_FROM T_AS
 T_INSTANCEOF T_TYPEOF
 T_DELETE  T_VOID
 T_TYPE  T_ANY_TYPE T_NUMBER_TYPE T_BOOLEAN_TYPE T_STRING_TYPE  T_ENUM
 T_DECLARE T_MODULE
 T_PUBLIC T_PRIVATE T_PROTECTED  T_READONLY
 
(*-----------------------------------------*)
(* Punctuation tokens *)
(*-----------------------------------------*)

(* syntax *)
%token <Parse_info.t>
 T_LCURLY "{" T_RCURLY "}"
 T_LPAREN "(" T_RPAREN ")"
 T_LBRACKET "[" T_RBRACKET "]"
 T_SEMICOLON ";" T_COMMA "," T_PERIOD "." T_COLON ":"
 T_PLING "?"
 T_ARROW "->" 
 T_BACKQUOTE 
 T_DOLLARCURLY
 (* regular JS token and also semgrep: *)
 T_DOTS "..."
 (* semgrep: *)
 LDots RDots


(* operators *)
%token <Parse_info.t>
 T_OR T_AND
 T_BIT_OR T_BIT_XOR T_BIT_AND
 T_PLUS T_MINUS
 T_DIV T_MULT "*" T_MOD
 T_NOT T_BIT_NOT 
 T_RSHIFT3_ASSIGN T_RSHIFT_ASSIGN T_LSHIFT_ASSIGN
 T_BIT_XOR_ASSIGN T_BIT_OR_ASSIGN T_BIT_AND_ASSIGN T_MOD_ASSIGN T_DIV_ASSIGN
 T_MULT_ASSIGN T_MINUS_ASSIGN T_PLUS_ASSIGN 
 T_ASSIGN "="
 T_EQUAL T_NOT_EQUAL T_STRICT_EQUAL T_STRICT_NOT_EQUAL
 T_LESS_THAN_EQUAL T_GREATER_THAN_EQUAL T_LESS_THAN T_GREATER_THAN
 T_LSHIFT T_RSHIFT T_RSHIFT3
 T_INCR T_DECR 
 T_EXPONENT

(*-----------------------------------------*)
(* XHP tokens *)
(*-----------------------------------------*)
%token <string * Parse_info.t> T_XHP_OPEN_TAG
(* The 'option' is for closing tags like </> *)
%token <string option * Parse_info.t> T_XHP_CLOSE_TAG

(* ending part of the opening tag *)
%token <Parse_info.t> T_XHP_GT T_XHP_SLASH_GT

%token <string * Parse_info.t> T_XHP_ATTR T_XHP_TEXT
(* '<>', see https://reactjs.org/docs/fragments.html#short-syntax *)
%token <Parse_info.t> T_XHP_SHORT_FRAGMENT

(*-----------------------------------------*)
(* Extra tokens: *)
(*-----------------------------------------*)

(* Automatically Inserted Semicolon (ASI), see parse_js.ml *)
%token <Parse_info.t> T_VIRTUAL_SEMICOLON
(* fresh_token: the opening '(' of the parameters preceding an '->' *)
%token <Parse_info.t> T_LPAREN_ARROW
(* fresh_token: the first '{' in a semgrep pattern for objects *)
%token <Parse_info.t> T_LCURLY_SEMGREP

(*************************************************************************)
(* Priorities *)
(*************************************************************************)

(* must be at the top so that it has the lowest priority *)
(* %nonassoc LOW_PRIORITY_RULE *)

(* Special if / else associativity*)
%nonassoc p_IF
%nonassoc T_ELSE

(* unused according to menhir:
%nonassoc p_POSTFIX
%right
 T_RSHIFT3_ASSIGN T_RSHIFT_ASSIGN T_LSHIFT_ASSIGN
 T_BIT_XOR_ASSIGN T_BIT_OR_ASSIGN T_BIT_AND_ASSIGN T_MOD_ASSIGN T_DIV_ASSIGN
 T_MULT_ASSIGN T_MINUS_ASSIGN T_PLUS_ASSIGN "="
*)

%left T_OR
%left T_AND
%left T_BIT_OR
%left T_BIT_XOR
%left T_BIT_AND
%left T_EQUAL T_NOT_EQUAL T_STRICT_EQUAL T_STRICT_NOT_EQUAL
%left T_LESS_THAN_EQUAL T_GREATER_THAN_EQUAL T_LESS_THAN T_GREATER_THAN
      T_IN T_INSTANCEOF
%left T_LSHIFT T_RSHIFT T_RSHIFT3
%left T_PLUS T_MINUS
%left T_DIV "*" T_MOD

%right T_EXPONENT
%right T_NOT T_BIT_NOT T_INCR T_DECR T_DELETE T_TYPEOF T_VOID T_AWAIT

(*************************************************************************)
(* Rules type decl *)
(*************************************************************************)
%start <Ast_js.program> main
%start <Ast_js.stmt option> module_item_or_eof 
%start <Ast_js.any> sgrep_spatch_pattern
(* for lang_json/ *)
%start <Ast_js.expr> json

(* just for better type error *)
%type <Ast_js.stmt list> stmt item module_item
%type <Ast_js.entity list> decl
%type <Parse_info.t> sc
%type <Ast_js.expr> element binding_elision_element binding_element
%type <Ast_js.property list> class_element
%type <Ast_js.property> binding_property

%%
(*************************************************************************)
(* Macros *)
(*************************************************************************)
listc(X):
 | X { (*[$1]*) raise Todo }
 | listc(X) "," X { (* $1 @ [$3] *) raise Todo }

optl(X):
 | (* empty *) { [] }
 | X           { $1 }

optl2(X):
 | (* empty *) { [] }
 | X           { List.flatten $1 }

(*************************************************************************)
(* Toplevel *)
(*************************************************************************)

main: program EOF { $1 }

(* less: could restrict to literals and collections *)
json: expr EOF { $1 }

(* TODO: use module_item* ? *)
program: optl2(module_item+) { $1 }

(* parse item by item, to allow error recovery and skipping some code *)
module_item_or_eof:
 | module_item { raise Todo } 
 | EOF         { None }

module_item:
 | item        { $1 }
 | import_decl { raise Todo (* $1 |> List.map (fun x -> M x)*) }
 | export_decl { raise Todo (* $1 |> List.map (fun x -> M x)*) }

(* item is also in stmt_list, inside every blocks *)
item:
 | stmt { $1 }
 | decl { $1 |> List.map (fun v -> DefStmt v) }

decl:
 (* part of hoistable_declaration in the ECMA grammar *)
 | function_decl  { raise Todo (* [ FunDecl $1] *) }
 (* es6: *)
 | generator_decl { raise Todo }
 (* es7: *)
 | async_decl     { raise Todo }

 (* es6: *)
 | lexical_decl   { $1 }
 | class_decl     { raise Todo (* [ ClassDecl $1] *) }

 (* typescript-ext: *)
 | interface_decl { raise Todo }
 | type_alias_decl { raise Todo }
 | enum_decl       { raise Todo }

(*************************************************************************)
(* sgrep *)
(*************************************************************************)

sgrep_spatch_pattern:
 (* copy-paste of object_literal rule but with T_LCURLY_SEMGREP *)
 | T_LCURLY_SEMGREP "}"    { Expr (Obj ($1, [], $2)) }
 | T_LCURLY_SEMGREP listc(property_name_and_value) ","? "}" 
     { Expr (Obj ($1, $2, $4)) }

 | assignment_expr_no_stmt  EOF  { Expr $1 }
 | module_item              EOF  { fix_sgrep_module_item $1}
 | module_item module_item+ EOF  { Items (List.flatten ($1::$2)) }

(*************************************************************************)
(* Namespace *)
(*************************************************************************)
(*----------------------------*)
(* import *)
(*----------------------------*)

import_decl: 
 | T_IMPORT import_clause from_clause sc  
    { (*$1, ImportFrom ($2, $3), $4*) raise Todo }
 | T_IMPORT module_specifier sc           
    { (*$1, ImportEffect $2, $3*) raise Todo }

import_clause: 
 | import_default                  { Some $1, None }
 | import_default "," import_names { Some $1, Some $3 }
 |                    import_names { None, Some $1 }

import_default: binding_id { $1 }

import_names:
 | "*" T_AS binding_id   { (* ImportNamespace ($1, $2, $3) *) raise Todo }
 | named_imports         { (*ImportNames $1*) raise Todo }
 (* typing-ext: *)
 | T_TYPE named_imports  { raise Todo }

named_imports:
 | "{" "}"                             { ($1, [], $2) }
 | "{" listc(import_specifier) "}"     { ($1, $2, $3) }
 | "{" listc(import_specifier) "," "}" { ($1, $2, $4) }

(* also valid for export *)
from_clause: T_FROM module_specifier { ($1, $2) }

import_specifier:
 | binding_id                 { $1, None }
 | id T_AS binding_id         { $1, Some ($2, $3) }
 (* not in ECMA, not sure what it means *)
 | T_DEFAULT T_AS binding_id  { ("default",$1), Some ($2, $3) }
 | T_DEFAULT                  { ("default",$1), None }

module_specifier: string_literal { $1 }

(*----------------------------*)
(* export *)
(*----------------------------*)

export_decl:
 | T_EXPORT export_names       { raise Todo (* $1, $2 *) }
 | T_EXPORT variable_stmt { raise Todo (*$1, ExportDecl (St $2)*) }
 | T_EXPORT decl        { raise Todo (*$1, ExportDecl $2*) }
 (* in theory just func/gen/class, no lexical_decl *)
 | T_EXPORT T_DEFAULT decl { raise Todo (* $1, ExportDefaultDecl ($2, $3) *) }
 | T_EXPORT T_DEFAULT assignment_expr_no_stmt sc 
    { raise Todo (* $1, ExportDefaultExpr ($2, $3, $4) *)  }
 (* ugly hack because should use assignment_expr above instead*)
 | T_EXPORT T_DEFAULT object_literal sc
    { raise Todo (* $1, ExportDefaultExpr ($2, Object $3, $4) *)  }


export_names:
 | "*"           from_clause sc { raise Todo(*ReExportNamespace ($1, $2, $3)*)}
 | export_clause from_clause sc { raise Todo (*ReExportNames ($1, $2, $3)*) }
 | export_clause sc             { raise Todo (*ExportNames ($1, $2)*) }

export_clause:
 | "{" "}"                              { ($1, [], $2) }
 | "{" listc(import_specifier) "}"      { ($1, $2, $3) }
 | "{" listc(import_specifier) ","  "}" { ($1, $2, $4) }

(*************************************************************************)
(* Variable decl *)
(*************************************************************************)

(* part of 'stmt' *)
variable_stmt: T_VAR listc(variable_decl) sc { build_vars  (Var, $1) $2 }

(* part of 'decl' *)
lexical_decl:
 (* es6: *)
 | T_CONST listc(variable_decl) sc { build_vars (Const, $1) $2 }
 | T_LET listc(variable_decl) sc   { build_vars (Let, $1) $2 }


(* one var from a list of vars *)
variable_decl:
 | id annotation? initializeur?               { Left $1, $2, $3 }
 | binding_pattern annotation? initializeur   { Right $1, $2, Some $3 }

initializeur: "=" assignment_expr { $2 }

initializeur2: "=" assignment_expr { $1, $2 }


for_variable_decl:
 | T_VAR listc(variable_decl_no_in)   { build_vars (Var, $1) $2 }
 (* es6: *)
 | T_CONST listc(variable_decl_no_in) { build_vars (Const, $1) $2 }
 | T_LET listc(variable_decl_no_in)   { build_vars (Let, $1) $2 }

variable_decl_no_in:
 | id initializer_no_in                 { Left $1, None, Some $2 }
 | id                                   { Left $1, None, None }
 | binding_pattern initializer_no_in    { Right $1, None, Some $2 }

(* 'for ... in' and 'for ... of' declare only one variable *)
for_single_variable_decl:
 | T_VAR for_binding   { build_var (Var, $1) $2 }
 (* es6: *)
 | T_CONST for_binding { build_var (Const, $1) $2 }
 | T_LET  for_binding  { build_var (Let, $1) $2 }

for_binding:
 | id   annotation?  { Left $1,  $2,   None }
 | binding_pattern   { Right $1, None, None }

(*----------------------------*)
(* pattern *)
(*----------------------------*)

binding_pattern:
 | object_binding_pattern { $1 }
 | array_binding_pattern  { $1 }

object_binding_pattern:
 | "{" "}"                               { Obj ($1, [], $2)  }
 | "{" listc(binding_property) ","?  "}" { Obj ($1, $2, $4) }

binding_property:
 | binding_id initializeur?          { mk_Field (PN $1) $2 }
 | property_name ":" binding_element { mk_Field $1 (Some $3) }
 (* can appear only at the end of a binding_property_list in ECMA *)
 | "..." binding_id      { FieldSpread ($1, mk_Id $2) }
 | "..." binding_pattern { FieldSpread ($1, $2) }

(* in theory used also for formal parameter as is *)
binding_element:
 | binding_id         initializeur2? { mk_pattern (mk_Id $1) $2 }
 | binding_pattern    initializeur2? { mk_pattern ($1)       $2 }

(* array destructuring *)

(* TODO use elision below.
 * invent a new Hole category or maybe an array_argument special 
 * type like for the (call)argument type.
 *)
array_binding_pattern:
 | "[" "]"                      { Arr ($1, [], $2) }
 | "[" binding_element_list "]" { Arr ($1, $2, $3) }

binding_start_element:
 | ","                  { [] (* TODO elision *) }
 | binding_element ","  { [$1] }

binding_start_list:
(* always ends in a "," *)
 | binding_start_element                     { $1 }
 | binding_start_list binding_start_element  { $1 @ $2 }

(* can't use listc() here, it's $1 not [$1] below *)
binding_element_list:
 | binding_start_list                         { $1 }
 | binding_elision_element                    { [$1] }
 | binding_start_list binding_elision_element { $1 @ [$2] }

binding_elision_element:
 | binding_element        { $1 }
 (* can appear only at the end of a binding_property_list in ECMA *)
 | "..." binding_id       { special Spread $1 [mk_Id $2] }
 | "..." binding_pattern  { special Spread $1 [$2] }

(*************************************************************************)
(* Function declarations (and exprs) *)
(*************************************************************************)

(* ugly: id is None only when part of an 'export default' decl 
 * less: use other tech to enforce this? extra rule after
 *  T_EXPORT T_DEFAULT? but then many ambiguities.
 *)
function_decl: T_FUNCTION id? call_signature "{" function_body "}"
   { $2, mk_Fun [] $3 ($4, $5, $6) }

(* the id is really optional here *)
function_expr: T_FUNCTION id? call_signature  "{" function_body "}"
   { mk_Fun ~id:$2 [] $3 ($4, $5, $6) }

(* typescript-ext: generics? and annotation? *)
call_signature: generics? "(" formal_parameter_list_opt ")"  annotation? 
  { $1, ($2, $3, $4), $5 }

function_body: optl(stmt_list) { $1 }

(*----------------------------*)
(* parameters *)
(*----------------------------*)

formal_parameter_list_opt:
 | (*empty*)                   { [] }
 | formal_parameter_list ","?  { List.rev $1 }

(* must be written in a left-recursive way (see conflicts.txt) *)
formal_parameter_list:
 | formal_parameter_list "," formal_parameter { $3::$1 }
 | formal_parameter                           { [$1] }

(* The ECMA and Typescript grammars imposes more restrictions
 * (some require_parameter, optional_parameter, rest_parameter)
 * but I've simplified.
 * We could also factorize with binding_element as done by ECMA.
 *)
formal_parameter:
 | id                { ParamClassic (mk_param $1) }
 (* es6: default parameter *)
 | id initializeur   { ParamClassic { (mk_param $1) with p_default = Some $2} }
  (* until here this is mostly equivalent to the 'binding_element' rule *)
  | binding_pattern annotation? initializeur2? 
    { ParamPattern (mk_pattern $1 $3) (* annotation? *) }

 (* es6: spread *)
 | "..." id          { ParamClassic { (mk_param $2) with p_dots = Some $1; } }

 (* typing-ext: *)
 | id annotation     { ParamClassic { (mk_param $1) with p_type = Some $2; } }
 (* TODO: token for '?' *)
 | id "?"            { ParamClassic (mk_param $1) }
 | id "?" annotation { ParamClassic { (mk_param $1) with p_type = Some $3 } }

 | id annotation initializeur
     { ParamClassic { (mk_param $1) with p_type = Some $2; p_default=Some $3}}

 | "..." id annotation
    { ParamClassic { (mk_param $2) with p_dots = Some $1; p_type = Some $3;} }
 (* sgrep-ext: *)
 | "..."              { Flag_parsing.sgrep_guard (ParamEllipsis $1) }

(*----------------------------*)
(* generators *)
(*----------------------------*)
generator_decl: T_FUNCTION "*" id? call_signature "{" function_body "}"
   { $3, mk_Fun [Generator, $2] $4 ($5, $6, $7) }

(* the id really is optional here *)
generator_expr: T_FUNCTION "*" id? call_signature "{" function_body "}"
   { mk_Fun ~id:$3 [Generator, $2] $4 ($5, $6, $7) }

(*----------------------------*)
(* asynchronous functions *)
(*----------------------------*)
async_decl: T_ASYNC T_FUNCTION id? call_signature "{" function_body "}"
   { $3, mk_Fun [Async, $1] $4 ($5, $6, $7) }

(* the id is really optional here *)
async_function_expr: T_ASYNC T_FUNCTION id? call_signature "{"function_body"}"
   { mk_Fun ~id:$3 [Async, $1] $4 ($5, $6, $7) }

(*************************************************************************)
(* Class declaration *)
(*************************************************************************)

(* ugly: c_name is None only when part of an 'export default' decl 
 * TODO: use other tech to enforce this? extra rule after
 * T_EXPORT T_DEFAULT? but then many ambiguities.
 *)
class_decl: T_CLASS binding_id? generics? class_heritage class_body
   { $2, mk_Class $1 $2 $3 $4 $5 }

(* TODO: use class_element* then? and List.flatten that? *)
class_body: "{" optl2(class_element+) "}" { ($1, $2, $3) }

class_heritage: extends_clause? optl(implements_clause)
  { Common.opt_to_list $1, $2 }

extends_clause: T_EXTENDS type_or_expr { raise Todo }
(* typescript-ext: *)
implements_clause: T_IMPLEMENTS listc(type_) { raise Todo }

binding_id: id { $1 }

class_expr: T_CLASS binding_id? generics? class_heritage class_body
   { mk_Class $1 $2 $3 $4 $5 }

(*----------------------------*)
(* Class elements *)
(*----------------------------*)

(* can't factorize with static_opt, or access_modifier_opt; ambiguities  *)
class_element:
 |                  method_definition  { [$1] }
 | access_modifiers method_definition  { [add_modifiers $1 $2] } 

 |                  property_name annotation? initializeur? sc 
    { [mk_Field $1 ~fld_type:$2 $3] }
 | access_modifiers property_name annotation? initializeur? sc 
    { [mk_Field ~props:$1 $2 ~fld_type:$3 $4] }

 | sc    { [] }
  (* sgrep-ext: enable class body matching *)
 | "..." { Flag_parsing.sgrep_guard ([FieldEllipsis $1]) }

(* TODO: cant use access_modifier+, conflict *)
access_modifiers: 
 | access_modifier                  { [$1] }
 | access_modifiers access_modifier { $1 @ [$2] }

(* less: should impose an order? *)
access_modifier:
 | T_STATIC    { Static, $1 }
 (* typescript-ext: *)
 | T_PUBLIC    { Public, $1 }
 | T_PRIVATE   { Private, $1 }
 | T_PROTECTED { Protected, $1 }

 | T_READONLY  { Readonly, $1 }

(*----------------------------*)
(* Method definition (in class or object literal) *)
(*----------------------------*)
method_definition:
 |     property_name call_signature "{" function_body "}"
    { mk_Field $1 (Some (mk_Fun [] $2 ($3, $4, $5))) }

 | "*" property_name call_signature "{" function_body "}"
    { mk_Field $2 (Some (mk_Fun [Generator, $1] $3 ($4, $5, $6))) }

 (* we enforce 0 parameter here *)
 | T_GET property_name generics? "(" ")" annotation? "{" function_body "}"
    { mk_Field $2 (Some (mk_Fun [Get, $1] ($3, ($4,[],$5), $6) ($7, $8, $9))) }
 (* we enforce 1 parameter here *)
 | T_SET property_name  generics? "(" formal_parameter ")" annotation?
    "{" function_body "}"
    { mk_Field $2 (Some (mk_Fun [Set, $1] ($3,($4,[$5],$6),$7) ($8,$9,$10))) }

 (* es7: *)
 | T_ASYNC property_name call_signature  "{" function_body "}"
  { mk_Field $2 (Some (mk_Fun [Async, $1] $3 ($4, $5, $6))) }

(*************************************************************************)
(* Interface declaration *)
(*************************************************************************)
(* typescript-ext: *)
(* TODO: use type_ at the end here and you get conflicts on '[' 
 * Why? because [] can follow an interface_decl? 
 *)

interface_decl: T_INTERFACE binding_id generics? interface_extends? object_type
   { raise Todo }

interface_extends: T_EXTENDS listc(type_reference) { raise Todo }

(*************************************************************************)
(* Type declaration *)
(*************************************************************************)
(* typescript-ext: *)
type_alias_decl: T_TYPE id "=" type_ sc { raise Todo }

enum_decl: T_CONST? T_ENUM id "{" listc(enum_member) ","? "}" { $7 }

enum_member:
 | property_name { }
 | property_name "=" assignment_expr_no_stmt { }

(*************************************************************************)
(* Declare (ambient) declaration *)
(*************************************************************************)
(* typescript-ext: *)

(*************************************************************************)
(* Types *)
(*************************************************************************)
(* typescript-ext: *)

(*----------------------------*)
(* Annotations *)
(*----------------------------*)

annotation: ":" type_ { raise Todo }

complex_annotation:
 | annotation { $1 }
 | generics? "(" optl(param_type_list) ")" ":" type_ 
     { raise Todo }

(*----------------------------*)
(* Types *)
(*----------------------------*)

(* can't use 'type'; generate syntax error in parser_js.ml *)
type_:
 | primary_or_union_type { $1 }
 | "?" type_         { raise Todo }
 | T_LPAREN_ARROW optl(param_type_list) ")" "->" type_ 
   { raise Todo }

primary_or_union_type:
 | primary_or_intersect_type { $1 }
 | union_type { $1 }

primary_or_intersect_type:
 | primary_type { $1 }
 | intersect_type { $1 }

(* I introduced those intermediate rules to remove ambiguities *)
primary_type:
 | primary_type2 { $1 }
 | primary_type "[" "]" { raise Todo }

primary_type2:
 | predefined_type      { G.TyName (G.name_of_id $1) }
 (* TODO: could be TyApply if snd $1 is a Some *)
 | type_reference       { G.TyName(G.name_of_ids (fst $1)) }
 | object_type          { $1 }
 | "[" listc(type_) "]" { raise Todo }
 (* not in Typescript grammar *)
 | T_STRING              { raise Todo }

predefined_type:
 | T_ANY_TYPE      { "any", $1 }
 | T_NUMBER_TYPE   { "number", $1 }
 | T_BOOLEAN_TYPE  { "boolean", $1 }
 | T_STRING_TYPE   { "string", $1 }
 | T_VOID          { "void", $1 }
 (* not in Typescript grammar, but often part of union type *)
 | T_NULL          { "null", $1 }

(* was called nominal_type in Flow *)
type_reference:
 | type_name { ($1,None) }
 | type_name type_arguments { ($1, Some $2) }

(* was called type_reference in Flow *)
type_name: 
 | T_ID { [$1] }
 | module_name "." T_ID { $1 @ [$3] }

module_name:
 | T_ID { [$1] }
 | module_name "." T_ID { $1 @ [$3] } 

union_type:     primary_or_union_type     T_BIT_OR primary_type { raise Todo }

intersect_type: primary_or_intersect_type T_BIT_AND primary_type { raise Todo }


object_type: "{" optl(type_member+) "}"  { raise Todo } 

(* partial type annotations are not supported *)
type_member: 
 | property_name_typescript complex_annotation sc_or_comma
    { raise Todo }
 | property_name_typescript "?" complex_annotation sc_or_comma
    { raise Todo }
 | "[" T_ID ":" T_STRING_TYPE "]" complex_annotation sc_or_comma
    { raise Todo  }
 | "[" T_ID ":" T_NUMBER_TYPE "]" complex_annotation sc_or_comma
    { raise Todo }

(* no [xxx] here *)
property_name_typescript:
 | id    { PN $1 }
 | string_literal  { PN $1 }
 | numeric_literal { PN $1 }
 | ident_keyword   { PN $1 }


param_type_list:
 | param_type "," param_type_list { $1::$3 }
 | param_type                         { [$1] }
 | optional_param_type_list           { $1 }

(* partial type annotations are not supported *)
param_type: id complex_annotation { raise Todo }

optional_param_type: id "?" complex_annotation { raise Todo }

optional_param_type_list:
 | optional_param_type "," optional_param_type_list { $1::$3 }
 | optional_param_type       { [$1] }
 | rest_param_type           { [$1] }

rest_param_type: "..." id complex_annotation { raise Todo }

(*----------------------------*)
(* Type parameters (type variables) *)
(*----------------------------*)

generics: T_LESS_THAN listc(type_parameter) T_GREATER_THAN { $1, $2, $3 }

type_parameter: T_ID { $1 }

(*----------------------------*)
(* Type arguments *)
(*----------------------------*)

type_arguments:
 | T_LESS_THAN listc(type_argument) T_GREATER_THAN { $1, $2, $3 }
 | mismatched_type_arguments { $1 }

type_argument: type_ { $1 }

(* a sequence of 2 or 3 closing > will be tokenized as >> or >>> *)
(* thus, we allow type arguments to omit 1 or 2 closing > to make it up *)
mismatched_type_arguments:
 | T_LESS_THAN type_argument_list1 T_RSHIFT  { $1, $2, $3 }
 | T_LESS_THAN type_argument_list2 T_RSHIFT3 { $1, $2, $3 }

type_argument_list1:
 | nominal_type1                              { [$1] }
 | listc(type_argument) "," nominal_type1     { $1 @ [$3]}

nominal_type1: type_name type_arguments1 { $1 }

(* missing 1 closing > *)
type_arguments1: T_LESS_THAN listc(type_argument)    { $1, $2, G.fake ">" }

type_argument_list2:
 | nominal_type2                            { [$1] }
 | listc(type_argument) "," nominal_type2   { $1 @ [$3]}

nominal_type2: type_name type_arguments2 { $1 }

(* missing 2 closing > *)
type_arguments2: T_LESS_THAN type_argument_list1    { $1, $2, G.fake ">" }

(*----------------------------*)
(* Type or expr *)
(*----------------------------*)

(* Extends arguments can be any expr according to ES6 *)
(* however, this causes ambiguities with type arguments a la TypeScript *)
(* unfortunately, TypeScript enforces severe restrictions here, *)
(* which e.g. do not admit mixins, which we want to support *)
(* TODO ambiguity Xxx.yyy, a Period expr or a module path in TypeScript *)
type_or_expr:
(* (* old: flow: *)
 | left_hand_side_expr_no_stmt { ($1,None) } 
 | type_name type_arguments { ($1, Some $2) }
*)
 (* typescript-ext: *)
 | type_reference { $1 }

(*************************************************************************)
(* Stmt *)
(*************************************************************************)

stmt:
 | block           { [$1] }
 | variable_stmt   { $1 |> List.map (fun x -> DefStmt x) }
 | empty_stmt      { [] }
 | expr_stmt       { [$1] }
 | if_stmt         { [$1] }
 | iteration_stmt  { [$1] }
 | continue_stmt   { [$1] }
 | break_stmt      { [$1] }
 | return_stmt     { [$1] }
 | with_stmt       { [$1] }
 | labelled_stmt   { [$1] }
 | switch_stmt     { [$1] }
 | throw_stmt      { [$1] }
 | try_stmt        { [$1] }
 (* sgrep-ext: *)
 | "..." { [ExprStmt (Ellipsis $1, G.sc)] }

%inline
stmt1: stmt { stmt1 $1 }

block: "{" optl(stmt_list) "}" { Block ($1, $2, $3) }

stmt_list: item+ { List.flatten $1 }

empty_stmt: sc { }

expr_stmt: expr_no_stmt sc { ExprStmt ($1, $2) (* less: generate a UseStrict*)}

if_stmt:
 | T_IF "(" expr ")" stmt1 T_ELSE stmt1 { If ($1, ($3), $5,Some($7)) }
 | T_IF "(" expr ")" stmt1 %prec p_IF   { If ($1, ($3), $5, None) }

iteration_stmt:
 | T_DO stmt1 T_WHILE "(" expr ")" sc   { Do ($1, $2, ($5)) }
 | T_WHILE "(" expr ")" stmt1           { While ($1, ($3), $5) }

 | T_FOR "(" expr_no_in ";" expr? ";" expr? ")" stmt1
     { For ($1, ForClassic (Right $3, $5, $7), $9) }
 | T_FOR "("            ";" expr? ";" expr? ")" stmt1
     { For ($1, ForClassic (Left [], $4, $6), $8) }
 | T_FOR "(" for_variable_decl ";" expr? ";" expr? ")" stmt1
     { For ($1, ForClassic (Left $3, $5, $7), $9) }

 | T_FOR "(" left_hand_side_expr T_IN expr ")" stmt1
     { For ($1, ForIn (Right $3, $4, $5), $7) }
 | T_FOR "(" for_single_variable_decl T_IN expr ")"  stmt1
     { For ($1, ForIn (Left $3, $4, $5), $7) }
 | T_FOR "(" left_hand_side_expr T_OF assignment_expr ")" stmt1
     { For ($1, ForOf (Right $3, $4, $5), $7) }
 | T_FOR "(" for_single_variable_decl T_OF assignment_expr ")"  stmt1
     { For ($1, ForOf (Left $3, $4, $5), $7) }
 (* sgrep-ext: *)
 | T_FOR "(" "..." ")"  stmt1
     { Flag_parsing.sgrep_guard (For ($1, ForEllipsis $3, $5)) }


initializer_no_in: "=" assignment_expr_no_in { $1, $2 }

continue_stmt: T_CONTINUE id? sc { Continue ($1, $2) }
break_stmt:    T_BREAK    id? sc { Break ($1, $2) }

return_stmt: T_RETURN expr? sc { Return ($1, $2) }

with_stmt: T_WITH "(" expr ")" stmt1 { With ($1, $3, $5) }

switch_stmt: T_SWITCH "(" expr ")" case_block { Switch ($1, $3, $5) }

labelled_stmt: id ":" stmt1 { Label ($1, $3) }


throw_stmt: T_THROW expr sc { Throw ($1, $2) }

try_stmt:
 | T_TRY block catch         { Try ($1, $2, Some $3, None)  }
 | T_TRY block       finally { Try ($1, $2, None, Some $3) }
 | T_TRY block catch finally { Try ($1, $2, Some $3, Some $4) }

catch:
 | T_CATCH "(" id ")" block              { BoundCatch ($1, mk_Id $3, $5) }
 | T_CATCH "(" binding_pattern ")" block { BoundCatch ($1, ($3), $5) }
 (* es2019 *)
 | T_CATCH block                         { UnboundCatch ($1, $2) }

finally: T_FINALLY block { $1, $2 }

(*----------------------------*)
(* auxillary stmts *)
(*----------------------------*)

(* TODO: use clase_clause* ? *)
case_block:
 | "{" optl(case_clause+) "}"
     { ($2) }
 | "{" optl(case_clause+) default_clause optl(case_clause+) "}"
     { ($2 @ [$3] @ $4) }

case_clause: T_CASE expr ":" optl(stmt_list)  { Case ($1, $2, stmt1 $4) }

default_clause: T_DEFAULT ":" optl(stmt_list) { Default ($1, stmt1 $3) }

(*************************************************************************)
(* Exprs *)
(*************************************************************************)

expr:
 | assignment_expr { $1 }
 | expr "," assignment_expr { seq ($1, $2, $3) }

(* coupling: see also assignment_expr_no_stmt and extend if can? *)
assignment_expr:
 | conditional_expr(d1) { $1 }
 | left_hand_side_expr_(d1) assignment_operator assignment_expr 
    { mk_Assign ($1,$2,$3) }

 (* es6: *)
 | arrow_function { $1 }
 (* es6: *)
 | T_YIELD                         { special Yield $1 [] }
 | T_YIELD     assignment_expr     { special Yield $1  [$2] }
 | T_YIELD "*" assignment_expr     { special YieldStar $1 [$3] }
 (* typescript-ext: 1.6, because <> cant be used in TSX files *)
 | left_hand_side_expr_(d1) T_AS type_ { $1 (* TODO $2 $3 *) }

 (* sgrep-ext: can't move in primary_expr, get s/r conflicts *)
 | "..." { Flag_parsing.sgrep_guard (Ellipsis $1) }
 | LDots expr RDots { Flag_parsing.sgrep_guard (DeepEllipsis ($1, $2, $3)) }

left_hand_side_expr: left_hand_side_expr_(d1) { $1 }

(*----------------------------*)
(* Generic part (to factorize rules) *)
(*----------------------------*)

conditional_expr(x):
 | post_in_expr(x) { $1 }
 | post_in_expr(x) "?" assignment_expr ":" assignment_expr
    { Conditional ($1, $3, $5) }

left_hand_side_expr_(x):
 | new_expr(x)  { $1 }
 | call_expr(x) { $1 }

post_in_expr(x):
 | pre_in_expr(x) { $1 }

 | post_in_expr(x) T_LESS_THAN post_in_expr(d1)          { bop G.Lt $1 $2 $3 }
 | post_in_expr(x) T_GREATER_THAN post_in_expr(d1)       { bop G.Gt $1 $2 $3 }
 | post_in_expr(x) T_LESS_THAN_EQUAL post_in_expr(d1)    { bop G.LtE $1 $2 $3 }
 | post_in_expr(x) T_GREATER_THAN_EQUAL post_in_expr(d1) { bop G.GtE $1 $2 $3 }
 | post_in_expr(x) T_INSTANCEOF post_in_expr(d1)  
    { special Instanceof $2 [$1; $3] }

 (* also T_IN! *)
 | post_in_expr(x) T_IN post_in_expr(d1)             { special In $2 [$1; $3] }

 | post_in_expr(x) T_EQUAL post_in_expr(d1)          { bop G.Eq $1 $2 $3 }
 | post_in_expr(x) T_NOT_EQUAL post_in_expr(d1)      { bop G.NotEq $1 $2 $3 }
 | post_in_expr(x) T_STRICT_EQUAL post_in_expr(d1)   { bop G.PhysEq $1 $2 $3 }
 | post_in_expr(x) T_STRICT_NOT_EQUAL post_in_expr(d1)   { bop G.NotPhysEq $1 $2 $3 }
 | post_in_expr(x) T_BIT_AND post_in_expr(d1)        { bop G.BitAnd $1 $2 $3 }
 | post_in_expr(x) T_BIT_XOR post_in_expr(d1)        { bop G.BitXor $1 $2 $3 }
 | post_in_expr(x) T_BIT_OR post_in_expr(d1)         { bop G.BitOr $1 $2 $3 }
 | post_in_expr(x) T_AND post_in_expr(d1)            { bop G.And $1 $2 $3 }
 | post_in_expr(x) T_OR post_in_expr(d1)             { bop G.Or $1 $2 $3 }


(* called unary_expr and update_expr in ECMA *)
pre_in_expr(x):
 | left_hand_side_expr_(x)                     { $1 }

 | pre_in_expr(x) T_INCR (* %prec p_POSTFIX*)    
    { special (IncrDecr (G.Incr, G.Postfix)) $2 [$1] }
 | pre_in_expr(x) T_DECR (* %prec p_POSTFIX*)    
    { special (IncrDecr (G.Decr, G.Postfix)) $2 [$1] }
 | T_INCR pre_in_expr(d1)                      
  { special (IncrDecr (G.Incr, G.Prefix)) $1 [$2] }
 | T_DECR pre_in_expr(d1)                      
  { special (IncrDecr (G.Decr, G.Prefix)) $1 [$2] }

 | T_DELETE pre_in_expr(d1)                    { special Delete $1 [$2] }
 | T_VOID pre_in_expr(d1)                      { special Void $1 [$2] }
 | T_TYPEOF pre_in_expr(d1)                    { special Typeof $1 [$2] }

 | T_PLUS pre_in_expr(d1)                      { uop (ArithOp G.Plus) $1 $2 }
 | T_MINUS pre_in_expr(d1)                     { uop (ArithOp G.Minus) $1 $2}
 | T_BIT_NOT pre_in_expr(d1)                   { uop (ArithOp G.BitNot) $1 $2 }
 | T_NOT pre_in_expr(d1)                       { uop (ArithOp G.Not) $1 $2 }
 (* es7: *)
 | T_AWAIT pre_in_expr(d1)                     { special Await $1 [$2] }

 | pre_in_expr(x) "*" pre_in_expr(d1)       { bop G.Mult $1 $2 $3 }
 | pre_in_expr(x) T_DIV pre_in_expr(d1)     { bop G.Div $1 $2 $3 }
 | pre_in_expr(x) T_MOD pre_in_expr(d1)     { bop G.Mod $1 $2 $3 }
 | pre_in_expr(x) T_PLUS pre_in_expr(d1)    { bop G.Plus $1 $2 $3 }
 | pre_in_expr(x) T_MINUS pre_in_expr(d1)   { bop G.Minus $1 $2 $3 }
 | pre_in_expr(x) T_LSHIFT pre_in_expr(d1)  { bop G.LSL $1 $2 $3 }
 | pre_in_expr(x) T_RSHIFT pre_in_expr(d1)  { bop G.LSR $1 $2 $3 }
 | pre_in_expr(x) T_RSHIFT3 pre_in_expr(d1) { bop G.ASR $1 $2 $3 }

 (* es7: *)
 | pre_in_expr(x) T_EXPONENT pre_in_expr(d1) { bop G.Pow $1 $2 $3 }


call_expr(x):
 | member_expr(x) arguments          { Apply ($1, $2) }
 | call_expr(x) arguments            { Apply ($1, $2) }
 | call_expr(x) "[" expr "]"         { ArrAccess ($1, ($2, $3,$4))}
 | call_expr(x) "." method_name      { ObjAccess ($1, $2, PN $3) }
 (* es6: *)
 | call_expr(x) template_literal     { mk_Encaps (Some $1) $2 }
 | T_SUPER arguments                 { Apply (mk_Super($1), $2) }

new_expr(x):
 | member_expr(x)    { $1 }
 | T_NEW new_expr(d1) { special New $1 [$2] }

member_expr(x):
 | primary_expr(x)                   { $1 }
 | member_expr(x) "[" expr "]"       { ArrAccess($1, ($2, $3, $4)) }
 | member_expr(x) "." field_name     { ObjAccess($1, $2, PN $3) }
 | T_NEW member_expr(d1) arguments   { Apply(special New $1 [$2], $3) }
 (* es6: *)
 | member_expr(x) template_literal   { mk_Encaps (Some $1) $2 }
 | T_SUPER "[" expr "]"              { ArrAccess(mk_Super($1),($2,$3,$4))}
 | T_SUPER "." field_name            { ObjAccess(mk_Super($1), $2, PN $3) }
 | T_NEW "." id { 
     if fst $3 = "target"
     then special NewTarget $1 []
     else raise (Parsing.Parse_error)
  }

primary_expr(x):
 | primary_expr_no_braces { $1 }
 | x { $1 }

d1: primary_with_stmt { $1 }

primary_with_stmt:
 | object_literal            { Obj $1 }
 | function_expr             { $1 }
 (* es6: *)
 | class_expr                { $1 }
 (* es6: *)
 | generator_expr            { $1 }
 (* es7: *)
 | async_function_expr       { $1 }


primary_expr_no_braces:
 | T_THIS          { IdSpecial (This, $1) }
 | id              { idexp_or_special $1 }

 | null_literal    { IdSpecial (Null, $1) }
 | boolean_literal { Bool $1 }
 | numeric_literal { Num $1 }
 | string_literal  { String $1 }

 (* marcel: this isn't an expansion of literal in ECMA-262... mistake? *)
 | regex_literal                { Regexp $1 }
 | array_literal                { $1 }

 (* simple! ECMA mixes this rule with arrow parameters (bad) *)
 | "(" expr ")" { $2 }

 (* xhp: do not put in 'expr', otherwise can't have xhp in function arg *)
 | xhp_html { Xml $1 }

 | template_literal { mk_Encaps None $1 }

(*----------------------------*)
(* scalar *)
(*----------------------------*)
boolean_literal:
 | T_TRUE  { true, $1 }
 | T_FALSE { false, $1 }

null_literal: T_NULL { $1 }
numeric_literal: T_NUMBER { $1 }
regex_literal: T_REGEX { $1 }
string_literal: T_STRING { $1 }

(*----------------------------*)
(* assign *)
(*----------------------------*)

assignment_operator:
 | "="              { $1, None }
 | T_MULT_ASSIGN    { $1, Some G.Mult }
 | T_DIV_ASSIGN     { $1, Some G.Div }
 | T_MOD_ASSIGN     { $1, Some G.Mod  }
 | T_PLUS_ASSIGN    { $1, Some G.Plus  }
 | T_MINUS_ASSIGN   { $1, Some G.Minus }
 | T_LSHIFT_ASSIGN  { $1, Some G.LSL }
 | T_RSHIFT_ASSIGN  { $1, Some G.LSR }
 | T_RSHIFT3_ASSIGN { $1, Some G.ASR  }
 | T_BIT_AND_ASSIGN { $1, Some G.BitAnd }
 | T_BIT_XOR_ASSIGN { $1, Some G.BitXor }
 | T_BIT_OR_ASSIGN  { $1, Some G.BitOr }

(*----------------------------*)
(* array *)
(*----------------------------*)

(* TODO: use elision below *)
array_literal:
 | "[" optl(elision) "]"                   { Arr($1, [], $3) }
 | "[" element_list_rev optl(elision) "]"  { Arr($1, List.rev $2, $4) }

(* TODO: conflict on ",", *)
element_list_rev:
 | optl(elision)   element               { [$2] }
 | element_list_rev "," element          { $3::$1 }
 | element_list_rev "," elision element  { $4::$1 }

element:
 | assignment_expr       { $1 }
 (* es6: spread operator: *)
 | "..." assignment_expr { special Spread $1 [$2] }

(*----------------------------*)
(* object *)
(*----------------------------*)

object_literal:
 | "{" "}"                                      { ($1, [], $2) }
 | "{" listc(property_name_and_value) ","? "}"  { ($1, $2, $4) }

property_name_and_value:
 | property_name ":" assignment_expr    { Field (mk_field $1 (Some $3)) }
 | method_definition                    { $1 }
 (* es6: *)
 | id           { Field (mk_field (PN $1) (Some (Id ($1, ref NotResolved)))) }
 (* es6: spread operator: *)
 | "..." assignment_expr                { (FieldSpread ($1, $2)) }
 | "..."                                { (FieldEllipsis $1 ) }

(*----------------------------*)
(* function call *)
(*----------------------------*)

arguments: "(" argument_list_opt ")" { ($1, $2, $3) }

argument_list_opt:
 | (*empty*)   { [] }
 (* argument_list must be written in a left-recursive way(see conflicts.txt) *)
 | listc(argument) ","?  { $1  }

(* assignment_expr because expr supports sequence of exprs with ',' *)
argument:
 | assignment_expr       { $1 }
 (* es6: spread operator, allowed not only in last position *)
 | "..." assignment_expr { special Spread $1 [$2] }

(*----------------------------*)
(* XHP embeded html *)
(*----------------------------*)
xhp_html:
 | T_XHP_OPEN_TAG xhp_attribute* T_XHP_GT xhp_child* T_XHP_CLOSE_TAG
     { { xml_tag = $1; xml_attrs = $2; xml_body = $4 } }
 | T_XHP_OPEN_TAG xhp_attribute* T_XHP_SLASH_GT
     { { xml_tag = $1; xml_attrs = $2; xml_body = [] } }
 (* reactjs-ext: https://reactjs.org/docs/fragments.html#short-syntax *)
 | T_XHP_SHORT_FRAGMENT xhp_child* T_XHP_CLOSE_TAG 
     { { xml_tag = ("", $1); xml_attrs = []; xml_body = $2 } }

xhp_child:
 | T_XHP_TEXT        { XmlText $1 }
 | xhp_html          { XmlXml $1 }
 | "{" expr sc? "}"  { XmlExpr ($2) }
 (* sometimes people use empty { } to put comment in it *)
 | "{" "}"           { XmlExpr (IdSpecial (Null, $1)) }

xhp_attribute:
 | T_XHP_ATTR "=" xhp_attribute_value { XmlAttr ($1, $3) }
 | "{" "..." assignment_expr "}" { XmlAttrExpr ($1, special Spread $2 [$3],$4)}
 (* reactjs-ext: see https://www.reactenlightenment.com/react-jsx/5.7.html *)
 | T_XHP_ATTR                         { XmlAttr ($1, Bool(true,G.fake "true"))}

xhp_attribute_value:
 | T_STRING           { String $1 }
 | "{" expr sc? "}"   { $2 }
 | "..."              { Ellipsis $1 }

(*----------------------------*)
(* interpolated strings *)
(*----------------------------*)
(* templated string (a.k.a interpolated strings) *)
template_literal: T_BACKQUOTE optl(encaps+) T_BACKQUOTE  { ($1, $2, $3) }

encaps:
 | T_ENCAPSED_STRING        { String $1 }
 | T_DOLLARCURLY expr "}"   { $2 }

(*----------------------------*)
(* arrow (short lambda) *)
(*----------------------------*)

(* TODO conflict with as then in indent_keyword_bis *)
arrow_function:
 (* es7: *)
 | T_ASYNC id T_ARROW arrow_body
     { mk_Fun [Async, $1] ((), fb [ParamClassic (mk_param $2)], None) $4 }
 | id T_ARROW arrow_body
     { mk_Fun [] ((), fb [ParamClassic (mk_param $1)], None) $3 }

 (* can not factorize with TOPAR parameter_list TCPAR, see conflicts.txt *)
 (* es7: *)
 | T_ASYNC T_LPAREN_ARROW formal_parameter_list_opt ")" annotation? T_ARROW arrow_body
    { mk_Fun [Async, $1] ((), ($2, $3, $4), $5) $7 }
 | T_LPAREN_ARROW formal_parameter_list_opt ")" annotation? T_ARROW arrow_body
    { mk_Fun [] ((), ($1, $2, $3), $4) $6 }


(* was called consise body in spec *)
arrow_body:
 | block  { match $1 with Block (a,b,c) -> (a,b,c) | _ -> raise Impossible }
 (* see conflicts.txt for why the %prec *)
 | assignment_expr_no_stmt (* %prec LOW_PRIORITY_RULE *) { mk_block_return $1 }
 (* ugly *)
 | function_expr { mk_block_return $1 }

(*----------------------------*)
(* no in *)
(*----------------------------*)
expr_no_in:
 | assignment_expr_no_in { $1 }
 | expr_no_in "," assignment_expr_no_in { seq ($1, $2, $3) }

assignment_expr_no_in:
 | conditional_expr_no_in { $1 }
 | left_hand_side_expr_(d1) assignment_operator assignment_expr_no_in
     { mk_Assign ($1, $2, $3) }

conditional_expr_no_in:
 | post_in_expr_no_in { $1 }
 | post_in_expr_no_in "?" assignment_expr_no_in ":" assignment_expr_no_in
     { Conditional ($1, $3, $5) }

post_in_expr_no_in:
 | pre_in_expr(d1) { $1 }
 | post_in_expr_no_in T_LESS_THAN post_in_expr(d1)        { bop G.Lt $1 $2 $3 }
 | post_in_expr_no_in T_GREATER_THAN post_in_expr(d1)     { bop G.Gt $1 $2 $3 }
 | post_in_expr_no_in T_LESS_THAN_EQUAL post_in_expr(d1)  { bop G.LtE $1 $2 $3 }
 | post_in_expr_no_in T_GREATER_THAN_EQUAL post_in_expr(d1) { bop G.GtE $1 $2 $3 }
 | post_in_expr_no_in T_INSTANCEOF post_in_expr(d1)     
   { special Instanceof $2 [$1; $3] }

 (* no T_IN case *)

 | post_in_expr_no_in T_EQUAL post_in_expr(d1)         { bop G.Eq $1 $2 $3 }
 | post_in_expr_no_in T_NOT_EQUAL post_in_expr(d1)     { bop G.NotEq $1 $2 $3 }
 | post_in_expr_no_in T_STRICT_EQUAL post_in_expr(d1)  { bop G.PhysEq $1 $2 $3}
 | post_in_expr_no_in T_STRICT_NOT_EQUAL post_in_expr(d1)   { bop G.NotPhysEq $1 $2 $3 }
 | post_in_expr_no_in T_BIT_AND post_in_expr(d1)       { bop G.BitAnd $1 $2 $3}
 | post_in_expr_no_in T_BIT_XOR post_in_expr(d1)       { bop G.BitXor $1 $2 $3}
 | post_in_expr_no_in T_BIT_OR post_in_expr(d1)        { bop G.BitOr $1 $2 $3 }
 | post_in_expr_no_in T_AND post_in_expr(d1)           { bop G.And $1 $2 $3 }
 | post_in_expr_no_in T_OR post_in_expr(d1)            { bop G.Or $1 $2 $3 }

(*----------------------------*)
(* (no stmt, and no object literal like { v: 1 }) *)
(*----------------------------*)
expr_no_stmt:
 | assignment_expr_no_stmt { $1 }
 | expr_no_stmt "," assignment_expr { seq ($1, $2, $3) }

assignment_expr_no_stmt:
 | conditional_expr(primary_no_stmt) { $1 }
 | left_hand_side_expr_(primary_no_stmt) assignment_operator assignment_expr
     { mk_Assign ($1, $2, $3) }
 (* es6: *)
 | arrow_function { $1 }
 (* es6: *)
 | T_YIELD                     { special Yield $1 [] }
 | T_YIELD assignment_expr     { special Yield $1 [$2] }
 | T_YIELD "*" assignment_expr { special YieldStar $1 [$3] }

(* no object_literal here *)
primary_no_stmt: TUnknown TComment { raise Impossible }

(*************************************************************************)
(* Entities, names *)
(*************************************************************************)
(* used for entities, parameters, labels, etc. *)
id:
 | T_ID               { $1 }
 | ident_semi_keyword { PI.str_of_info $1, $1 }

(* add here keywords which are not considered reserved by ECMA *)
ident_semi_keyword:
 | T_FROM { $1 } | T_OF { $1 }
 | T_GET { $1 } | T_SET { $1 }
 | T_CONSTRUCTOR { $1 }
 | T_TYPE { $1 }
 | T_ANY_TYPE { $1 } | T_NUMBER_TYPE { $1 } | T_BOOLEAN_TYPE { $1 }
 | T_STRING_TYPE { $1 }
 | T_DECLARE { $1 }
 | T_MODULE { $1 }
 | T_PUBLIC { $1 } | T_PRIVATE { $1 } | T_PROTECTED { $1 } | T_READONLY { $1 }
 (* can have AS and ASYNC here but need to restrict arrow_function then *)
 | T_AS { $1 }
 | T_ASYNC { $1 }
 (* TODO: would like to add T_IMPORT here, but cause conflicts *)

(* alt: use the _last_non_whitespace_like_token trick and look if
 * previous token was a period to return a T_ID
 *)
ident_keyword: ident_keyword_bis { PI.str_of_info $1, $1 }

ident_keyword_bis:
 | T_FUNCTION { $1 } | T_CONST { $1 } | T_VAR { $1 } | T_LET { $1 }
 | T_IF { $1 } | T_ELSE { $1 }
 | T_WHILE { $1 } | T_FOR { $1 } | T_DO { $1 }
 | T_CONTINUE { $1 } | T_BREAK { $1 }
 | T_SWITCH { $1 } | T_CASE { $1 } | T_DEFAULT { $1 }
 | T_RETURN { $1 }
 | T_THROW { $1 } | T_TRY { $1 } | T_CATCH { $1 } | T_FINALLY { $1 }
 | T_YIELD { $1 } | T_AWAIT { $1 }
 | T_NEW { $1 } | T_IN { $1 } | T_INSTANCEOF { $1 } | T_DELETE { $1 }
 | T_THIS { $1 } | T_SUPER { $1 }
 | T_WITH { $1 }
 | T_NULL { $1 }
 | T_FALSE { $1 } | T_TRUE { $1 }
 | T_CLASS { $1 } | T_INTERFACE { $1 } | T_EXTENDS { $1 } | T_STATIC { $1 }
 | T_IMPORT { $1 } | T_EXPORT { $1 } 
 | T_ENUM { $1 }
 | T_TYPEOF { $1 } | T_VOID { $1 }

field_name:
 | id            { $1 }
 | ident_keyword { $1 }

method_name:
 | id            { $1 }
 | ident_keyword { $1 }

property_name:
 | id              { PN $1 }
 | string_literal  { PN $1 }
 | numeric_literal { PN $1 }
 | ident_keyword   { PN $1 }
 (* es6: *)
 | "[" assignment_expr "]" { PN_Computed ($2) }


(*************************************************************************)
(* Misc *)
(*************************************************************************)
sc:
 | ";"                 { $1 }
 | T_VIRTUAL_SEMICOLON { $1 }

sc_or_comma:
 | sc  { raise Todo }
 | "," { raise Todo }

elision:
 | "," { [$1] }
 | elision "," { $1 @ [$2] }
