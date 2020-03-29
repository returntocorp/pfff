%{
(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)
open Common

open Cst_ml
(*************************************************************************)
(* Prelude *)
(*************************************************************************)
(* This file contains a grammar for OCaml (3.07 with some extensions for 
 * OCaml 4.xxx)
 * 
 * src: adapted from the official source of OCaml in its
 * parsing/ subdirectory. All semantic actions are new. Only the
 * grammar structure was copied.
 * was: $Id: parser.mly 10536 2010-06-07 15:32:32Z doligez $
 *
 * reference:
 * - http://caml.inria.fr/pub/docs/manual-ocaml/language.html
 *   (note that it unfortunately contains conflicts when translated into yacc).
 * 
 * other sources:
 * - http://www.cs.ru.nl/~tews/htmlman-3.10/full-grammar.html
 *   itself derived from the official ocaml reference manual
 *   (also contains conflicts when translated into yacc).
 * - http://www.mpi-sws.org/~rossberg/sml.html
 *   (also contains conflicts when translated into yacc).
 * - http://www.mpi-sws.org/~rossberg/hamlet/
 *   solves ambiguities
 * - linear-ML parser
 * 
 * alternatives: 
 *   - use menhir ? 
 *   - use dypgen ?
 *)

let (qufix: long_name -> tok -> (string wrap) -> long_name) = 
 fun longname dottok ident ->
  match longname with
  | xs, Name ident2 ->
      xs @ [Name ident2, dottok], Name ident

let to_item xs =
  xs |> Common.map_filter (function
  | TopItem x -> Some x
  | _ -> None
  )
%}
(*************************************************************************)
(* Tokens *)
(*************************************************************************)

(* unrecognized token, will generate parse error *)
%token <Parse_info.t> TUnknown

%token <Parse_info.t> EOF

(*-----------------------------------------*)
(* The space/comment tokens *)
(*-----------------------------------------*)

(* coupling: Token_helpers.is_real_comment *)
%token <Parse_info.t> TCommentSpace TCommentNewline   TComment
%token <Parse_info.t> TCommentMisc

(*-----------------------------------------*)
(* The normal tokens *)
(*-----------------------------------------*)

(* tokens with "values" *)
%token <string * Parse_info.t> TInt TFloat TChar TString
%token <string * Parse_info.t> TLowerIdent TUpperIdent
%token <string * Parse_info.t> TLabelUse TLabelDecl TOptLabelUse TOptLabelDecl

(* keywords tokens *)
%token <Parse_info.t>
 Tfun Tfunction Trec Ttype Tof Tif Tthen Telse
 Tmatch Twith Twhen
 Tlet Tin Tas
 Ttry Texception
 Tbegin Tend Tfor Tdo Tdone Tdownto Twhile Tto
 Tval Texternal
 Ttrue Tfalse
 Tmodule Topen Tfunctor Tinclude Tsig Tstruct
 Tclass Tnew Tinherit Tconstraint Tinitializer Tmethod Tobject Tprivate
 Tvirtual
 Tlazy Tmutable Tassert
 Tand 
 Tor Tmod Tlor Tlsl Tlsr Tlxor Tasr Tland

(* syntax *)
%token <Parse_info.t> 
 TOParen "(" TCParen ")" TOBrace "{" TCBrace "}" TOBracket "[" TCBracket "]"
 TOBracketPipe "[|" TPipeCBracket "|]"  TOBracketLess "[<" TGreaterCBracket ">]"
 TOBraceLess "{<" TGreaterCBrace ">}"
 TOBracketGreater "[>" TColonGreater ":>"
 TDot "." TDotDot ".."
 TComma "," TEq "=" TAssign ":=" TAssignMutable "<-" 
 TColon ":" TColonColon "::"
 TBang "!" TBangEq "!=" TTilde "~" TPipe "|"
 TSemiColon ";" TSemiColonSemiColon ";;"
 TQuestion "?" TQuestionQuestion "??"
 TUnderscore "_" TStar "*" TArrow "->" TQuote "'" TBackQuote "`" 
 TAnd TAndAnd 
 TSharp "#"
 TMinusDot TPlusDot

(* operators *)
%token <Parse_info.t> TPlus TMinus TLess TGreater
%token <string * Parse_info.t> TPrefixOperator TInfixOperator

(* attributes *)
%token <Parse_info.t> TBracketAt TBracketAtAt TBracketAtAtAt
%token <Parse_info.t> TBracketPercent TBracketPercentPercent

(*-----------------------------------------*)
(* extra tokens: *)
(*-----------------------------------------*)
%token <Parse_info.t> TSharpDirective

(*************************************************************************)
(* Priorities *)
(*************************************************************************)
(*
(* Precedences and associativities.
 *
 * Tokens and rules have precedences.  A reduce/reduce conflict is resolved
 * in favor of the first rule (in source file order).  A shift/reduce conflict
 * is resolved by comparing the precedence and associativity of the token to
 * be shifted with those of the rule to be reduced.
 * 
 * By default, a rule has the precedence of its rightmost terminal (if any).
 * 
 * When there is a shift/reduce conflict between a rule and a token that
 * have the same precedence, it is resolved using the associativity:
 * if the token is left-associative, the parser will reduce; if
 * right-associative, the parser will shift; if non-associative,
 * the parser will declare a syntax error.
 * 
 * We will only use associativities with operators of the kind  x * x -> x
 * for example, in the rules of the form    expr: expr BINOP expr
 * in all other cases, we define two precedences if needed to resolve
 * conflicts.
 * 
 * The precedences must be listed from low to high.
 *)
*)

%nonassoc Tin
%nonassoc below_SEMI
%nonassoc TSemiColon                     (* below TEq ({lbl=...; lbl=...}) *)
%nonassoc Tlet                           (* above TSemiColon ( ...; let ... in ...) *)
%nonassoc below_WITH
%nonassoc Tfunction Twith                 (* below TPipe  (match ... with ...) *)
%nonassoc Tand             (* above Twith (module rec A: Tsig with ... and ...) *)
%nonassoc Tthen                          (* below Telse (if ... then ...) *)
%nonassoc Telse                          (* (if ... then ... else ...) *)
%nonassoc TAssignMutable                 (* below TAssign (lbl <- x := e) *)
%right    TAssign                        (* expr (e := e := e) *)
%nonassoc Tas
%left     TPipe                          (* pattern (p|p|p) *)
%nonassoc below_COMMA
%left     TComma                         (* expr/expr_comma_list (e,e,e) *)
%right    TArrow                         (* core_type2 (t -> t -> t) *)
%right    Tor BARBAR                     (* expr (e || e || e) *)
%right    TAnd TAndAnd                   (* expr (e && e && e) *)
%nonassoc below_EQUAL
%left     INFIXOP0 TEq TLess TGreater    (* expr (e OP e OP e) *)
%left     TBangEq
%right    INFIXOP1                       (* expr (e OP e OP e) *)
%right    TColonColon                    (* expr (e :: e :: e) *)
%left     INFIXOP2 TPlus TPlusDot TMinus TMinusDot  (* expr (e OP e OP e) *)
%left     INFIXOP3 TStar                 (* expr (e OP e OP e) *)
%left     TInfixOperator (* pad: *)
%right    INFIXOP4                       (* expr (e OP e OP e) *)
%left     Tmod Tlor Tlxor Tland
%right    Tlsr Tasr Tlsl

%nonassoc prec_unary_minus prec_unary_plus (* unary - *)
%nonassoc prec_constant_constructor      (* cf. simple_expr (C versus C x) *)
%nonassoc prec_constr_appl               (* above Tas TPipe TColonColon TComma *)
%nonassoc below_SHARP
%nonassoc TSharp                         (* simple_expr/toplevel_directive *)
%nonassoc below_DOT
%nonassoc TDot
(* Finally, the first tokens of simple_expr are above everything else. *)
%nonassoc TBackQuote TBang Tbegin TChar Tfalse TFloat TInt
          TOBrace TOBraceLess TOBracket TOBracketPipe TLowerIdent TOParen
          Tnew TPrefixOperator TString Ttrue TUpperIdent

(*************************************************************************)
(* Rules type declaration *)
(*************************************************************************)

%start interface  implementation
%type <Cst_ml.toplevel list> interface
%type <Cst_ml.toplevel list> implementation

%%

(*************************************************************************)
(* TOC *)
(*************************************************************************)
(*
(*
 * - toplevel
 * - signature
 * - structure
 * - names
 * 
 * - expression
 * - type
 * - pattern
 * with for the last 3 sections subsections around values:
 *    - constants
 *    - constructors
 *    - lists
 *    - records
 *    - tuples 
 *    - arrays
 *    - name tags (`Foo)
 * 
 * - let/fun
 * - classes (not in AST)
 * - modules
 * - attributes
 * - xxx_opt, xxx_list
 * 
 *)
*)
(*************************************************************************)
(* Toplevel, compilation units *)
(*************************************************************************)

interface:      signature EOF                        { $1 }

implementation: structure EOF                        { $1 }

(*************************************************************************)
(* Signature *)
(*************************************************************************)

signature:
 | (* empty *)                                  { [] }
 | signature signature_item                     { $1 @ [TopItem $2] }
 | signature signature_item ";;" { $1 @ [TopItem $2; ScSc $3] }

signature_item: signature_item_noattr post_item_attributes { $1 }

signature_item_noattr:
 | Ttype type_declarations
     { Type ($1, $2) }
 | Tval val_ident ":" core_type
     { Val ($1, Name $2, $3, $4) }
 | Texternal val_ident ":" core_type "=" primitive_declaration
     { External ($1, Name $2, $3, $4, $5, $6) }
 | Texception TUpperIdent constructor_arguments
     { Exception ($1, Name $2, $3) }

 (* modules *)
 | Topen mod_longident
     { Open ($1, $2) }

 | Tmodule Ttype ident "=" module_type
     { ItemTodo $1 }
 | Tmodule TUpperIdent module_declaration
     { ItemTodo $1 }

 (* objects *)
 | Tclass class_descriptions
      { ItemTodo $1 }

(*----------------------------*)
(* Misc *)
(*----------------------------*)

primitive_declaration:
 | TString                                      { [$1] }
 | TString primitive_declaration                { $1::$2 }

(*************************************************************************)
(* Structure *)
(*************************************************************************)

(* pad: should not allow those toplevel seq_expr *)
structure:
 | structure_tail                              { $1 }
 | seq_expr structure_tail                     { TopSeqExpr $1::$2 }

structure_tail:
 |  (* empty *)                                 
     { [] }
 | ";;"                          
     { [ScSc $1] }
 | ";;" seq_expr structure_tail  
     { ScSc $1::TopSeqExpr $2::$3 }
 | ";;" structure_item structure_tail  
     { ScSc $1::TopItem $2::$3 }
 | ";;" TSharpDirective  structure_tail  
     { ScSc $1::TopDirective $2::$3 }

 | structure_item structure_tail                      
     { TopItem $1::$2 }
 | TSharpDirective structure_tail 
     { TopDirective $1::$2 }

structure_item: structure_item_noattr post_item_attributes { $1 }

structure_item_noattr:
 (* as in signature_item *)
 | Ttype type_declarations
     { Type ($1, $2) }
 | Texception TUpperIdent constructor_arguments
     { Exception ($1, Name $2, $3) }
 | Texternal val_ident ":" core_type "=" primitive_declaration
     { External ($1, Name $2, $3, $4, $5, $6)  }

 | Topen mod_longident
      { Open ($1, $2) }

 (* start of deviation *)
 | Tlet rec_flag let_bindings
      { Let ($1, $2, $3) }


 (* modules *)
 | Tmodule TUpperIdent module_binding
      { 
        match $3 with
        | None -> ItemTodo $1
        | Some (x, y) ->
            Module ($1, Name $2, x, y) 
      }
 | Tmodule Ttype ident "=" module_type
      { ItemTodo $1 }
 | Tinclude module_expr
      { ItemTodo $1 }

 (* objects *)
  | Tclass class_declarations
      { ItemTodo $1 }
  | Tclass Ttype class_type_declarations
      { ItemTodo $1 }

(*************************************************************************)
(* Names *)
(*************************************************************************)

val_ident:
 | TLowerIdent                                { $1 }
 | "(" operator ")"                   { ("TODOOPERATOR", $1) }

operator:
 | TPrefixOperator      { } | TInfixOperator       { }
 | "*"     { } | "="       { } | ":="   { } | "!"     { }
  (* but not Tand, because of conflict ? *)
 | Tor       { } | TAnd      { }
 | Tmod      { } | Tland     { } | Tlor      { } | Tlxor     { }
 | Tlsl      { } | Tlsr      { } | Tasr      { }
 | TPlus     { } | TPlusDot  { } | TMinus    { } | TMinusDot { }
 | TLess     { } | TGreater  { }
 | TAndAnd { } | TBangEq { }

(* for polymorphic types both 'a and 'A is valid. Same for module types. *)
ident:
 | TUpperIdent                                      { $1 }
 | TLowerIdent                                      { $1 }


constr_ident:
 | TUpperIdent     { $1 }
 | "(" ")" { "()TODO", $1 }
 | "::"     { "::", $1 }
 | Tfalse          { "false", $1 }
 | Ttrue           { "true", $1 }
(*  | "[" "]"                           { } *)
(*  | "(" "::" ")"                    { "::" } *)

(* record field name *)
label: TLowerIdent  { $1 }

name_tag: "`" ident   { }

(*----------------------------*)
(* Labels *)
(*----------------------------*)

label_var:
    TLowerIdent    { }

(* for label arguments like ~x or ?x *)
label_ident:
    TLowerIdent   { $1 }

 
(*----------------------------*)
(* Qualified names *)
(*----------------------------*)

mod_longident:
 | TUpperIdent                       { [], Name $1 }
 | mod_longident "." TUpperIdent    { qufix $1 $2 $3 }

mod_ext_longident:
 | TUpperIdent                                  { [], Name $1 }
 | mod_ext_longident "." TUpperIdent           { qufix $1 $2 $3 }
 | mod_ext_longident "(" mod_ext_longident ")" 
     { [], Name ("TODOEXTMODULE", $2) }



type_longident:
 | TLowerIdent                               { [], Name $1 }
 | mod_ext_longident "." TLowerIdent        { qufix $1 $2 $3 }

val_longident:
 | val_ident                                   { [], Name $1 }
 | mod_longident "." val_ident                { qufix $1 $2 $3 }

constr_longident:
 | mod_longident       %prec below_DOT     { $1 }
 | "[" "]"                     { [], Name ("[]TODO", $1) }
 | "(" ")"                         { [], Name ("()TODO", $1) }
 | Tfalse                                  { [], Name ("false", $1) }
 | Ttrue                                   { [], Name ("true", $1) }

(* record field name *)
label_longident:
 | TLowerIdent                              { [], Name $1 }
 | mod_longident "." TLowerIdent           { qufix $1 $2 $3 }


class_longident:
 | TLowerIdent                               { [], Name $1 }
 | mod_longident "." TLowerIdent            { qufix $1 $2 $3 }

mty_longident:
 | ident                                      { [], Name $1 }
 | mod_ext_longident "." ident               { qufix $1 $2 $3 }

(* it's mod_ext_longident, not mod_longident *)
clty_longident:
 | TLowerIdent                               { [], Name $1 }
 | mod_ext_longident "." TLowerIdent            { qufix $1 $2 $3 }

(*************************************************************************)
(* Expressions *)
(*************************************************************************)

seq_expr:
 | expr             %prec below_SEMI   { [Left $1] }
 | expr ";" seq_expr            { Left $1::Right $2::$3 }

 (* bad ? should be removed ? but it's convenient in certain contexts like
    * begin end to allow ; as a terminator
    *)
 | expr ";"                     { [Left $1; Right $2] }


expr:
 | simple_expr %prec below_SHARP
     { $1 }
 (* function application *)
 | simple_expr simple_labeled_expr_list
     { match $1 with
       | L name -> FunCallSimple (name, $2)
       | _      -> FunCall ($1, $2)
     }

 | Tlet rec_flag let_bindings Tin seq_expr
     { LetIn ($1, $2, $3, $4, $5) }

 | Tfun labeled_simple_pattern fun_def
     { let (params, action) = $3 in
       Fun ($1, $2::params, action)
     }

 | Tfunction opt_bar match_cases
     { Function ($1, $2 @ $3) }

 | expr_comma_list %prec below_COMMA
     { Tuple $1 }
 | constr_longident simple_expr %prec below_SHARP
     { Constr ($1, Some $2) }

 | expr "::" expr
     { Infix ($1, ("::", $2), $3) (* TODO ? ConsList ? *) }

 | expr TInfixOperator expr
     { Infix ($1, $2, $3) }

 | expr Tmod expr 
     { Infix ($1, ("mod", $2), $3) }
 | expr Tland expr 
     { Infix ($1, ("land", $2), $3) }
 | expr Tlor expr 
     { Infix ($1, ("lor", $2), $3) }
 | expr Tlxor expr 
     { Infix ($1, ("lxor", $2), $3) }

 | expr Tlsl expr 
     { Infix ($1, ("lsl", $2), $3) }
 | expr Tlsr expr 
     { Infix ($1, ("lsr", $2), $3) }
 | expr Tasr expr 
     { Infix ($1, ("asr", $2), $3) }

 | expr TBangEq expr
     { Infix ($1, ("!=", $2), $3) }

(*
  | expr INFIXOP1 expr
      { mkinfix $1 $2 $3 }
  | expr INFIXOP2 expr
      { mkinfix $1 $2 $3 }
  | expr INFIXOP3 expr
      { mkinfix $1 $2 $3 }
  | expr INFIXOP4 expr
      { mkinfix $1 $2 $3 }
*)

 | Tif seq_expr Tthen expr Telse expr
     { If ($1, $2, $3, $4, Some ($5, $6)) }
 | Tif seq_expr Tthen expr
     { If ($1, $2, $3, $4, None) }

 | Tmatch seq_expr Twith opt_bar match_cases
     { Match ($1, $2, $3, $4 @ $5) }

 | Ttry seq_expr Twith opt_bar match_cases
     { Try ($1, $2, $3, $4 @ $5) }

 | Twhile seq_expr Tdo seq_expr Tdone
     { While ($1, $2, $3, $4, $5) }
 | Tfor val_ident "=" seq_expr direction_flag seq_expr Tdo seq_expr Tdone
     { For ($1, Name $2, $3, $4, $5, $6, $7, $8, $9)  }

 | expr ":=" expr { RefAssign ($1, $2, $3) }

 | expr "=" expr   { Infix ($1, ("=", $2), $3) }

 | expr TPlus expr     { Infix ($1, ("+", $2), $3)  }
 | expr TMinus expr    { Infix ($1, ("-", $2), $3) }
 | expr TPlusDot expr  { Infix ($1, ("+.", $2), $3) }
 | expr TMinusDot expr { Infix ($1, ("-.", $2), $3) }
 | expr "*" expr     { Infix ($1, ("*", $2), $3) }
 | expr TLess expr     { Infix ($1, ("<", $2), $3) }
 | expr TGreater expr  { Infix ($1, (">", $2), $3) }
 | expr Tor expr       { Infix ($1, ("or", $2), $3) }
 | expr TAnd expr      { Infix ($1, ("&", $2), $3) }
 | expr TAndAnd expr   { Infix ($1, ("&&", $2), $3) }

 | subtractive expr %prec prec_unary_minus
     { Prefix ($1, $2) }
 | additive expr %prec prec_unary_plus
     { Prefix ($1, $2) }

 | simple_expr "." label_longident "<-" expr
      { FieldAssign ($1, $2, $3, $4, $5) }


 (* array extension *)
 | simple_expr "." "(" seq_expr ")" "<-" expr
     { ExprTodo }
 | simple_expr "." "[" seq_expr "]" "<-" expr
     { ExprTodo }
 (* bigarray extension, a.{i} <- v *)
 | simple_expr "." "{" expr "}" "<-" expr
     { ExprTodo }
     

 | Tlet Topen mod_longident Tin seq_expr
      { ExprTodo }

 | Tassert simple_expr %prec below_SHARP
     { ExprTodo }

 | name_tag simple_expr %prec below_SHARP
     { ExprTodo }

 | Tlazy simple_expr %prec below_SHARP
     { ExprTodo }

  (* objects *)
  | label "<-" expr
      { ExprTodo }


simple_expr:
 | constant
     { C $1 }
 | val_longident
     { L $1 }
 (* this includes 'false' *)
 | constr_longident %prec prec_constant_constructor
     { Constr ($1, None) }

 | simple_expr "." label_longident
     { FieldAccess ($1, $2, $3) }

 (* if only one expr then prefer to generate a ParenExpr *)
 | "(" seq_expr ")"
     { match $2 with
     | [] -> Sequence ($1, $2, $3) 
     | [Left x] -> ParenExpr ($1, x, $3)
     | [Right _] -> raise Impossible
     | _ -> Sequence ($1, $2, $3) 
     }

 | Tbegin seq_expr Tend
     { Sequence ($1, $2, $3)  }
 | Tbegin Tend
     { Sequence ($1, [], $2) }

 (* bugfix: must be in simple_expr. Originally made the mistake to put it
    * in expr: and the parser would then not recognize things like 'foo !x'
    *)
 | TPrefixOperator simple_expr
     { Prefix ($1, $2) }
 | "!" simple_expr
     { RefAccess ($1, $2) }


 | "{" record_expr "}"
     { Record ($1, $2, $3) }

 | "[" expr_semi_list opt_semi3 "]"
     { List ($1, $2 @ $3, $4) }

 | "[|" expr_semi_list opt_semi "|]"
     { ExprTodo }
 | "[|" "|]"
     { ExprTodo }

 (* array extension *)
 | simple_expr "." "(" seq_expr ")"
     { ExprTodo }
 | simple_expr "." "[" seq_expr "]"
     { ExprTodo }
 (* bigarray extension *)
 | simple_expr "." "{" expr "}"
     { ExprTodo }

 (* object extension *)
 | simple_expr "#" label
     { ObjAccess ($1, $2, Name $3) }
 | Tnew class_longident
     { New ($1, $2) }

 | "{<" field_expr_list opt_semi ">}"
      { ExprTodo }


 (* name tag extension *)
 | name_tag %prec prec_constant_constructor
     { ExprTodo }

 | "(" seq_expr type_constraint ")"
     { ExprTodo }

 (* scoped open, 3.12 *)
 | mod_longident "." "(" seq_expr ")"
     { ExprTodo }

simple_labeled_expr_list:
 | labeled_simple_expr
      { [$1] }
 | simple_labeled_expr_list labeled_simple_expr
      { $1 @ [$2] }

labeled_simple_expr:
 | simple_expr %prec below_SHARP
      { ArgExpr $1 }
 | label_expr
      { $1 }


expr_comma_list:
 | expr_comma_list "," expr                  { $1 @ [Right $2; Left $3] }
 | expr "," expr                             { [Left $1; Right $2; Left $3] }

expr_semi_list:
 | expr                                  { [Left $1] }
 | expr_semi_list ";" expr        { $1 @ [Right $2; Left $3] }





record_expr:
 | lbl_expr_list opt_semi                    { RecordNormal ($1 @ $2) }
 | simple_expr Twith lbl_expr_list opt_semi  { RecordWith ($1, $2, $3 @ $4) }

lbl_expr_list:
 | label_longident "=" expr
     { [Left (FieldExpr ($1, $2, $3))] }
 | lbl_expr_list ";"     label_longident "=" expr
     { $1 @ [Right $2; Left (FieldExpr ($3, $4, $5))] }
 (* new 3.12 feature! *)
 | label_longident
      { [Left (FieldImplicitExpr ($1))] }
 | lbl_expr_list ";"     label_longident
     { $1 @ [Right $2; Left (FieldImplicitExpr $3)] }


subtractive:
  | TMinus                                       { "-", $1 }
  | TMinusDot                                    { "-.", $1 }

additive:
  | TPlus                                        { "+", $1 }
  | TPlusDot                                     { "+.", $1 }


direction_flag:
 | Tto                                          { To $1 }
 | Tdownto                                      { Downto $1 }


(*----------------------------*)
(* Constants *)
(*----------------------------*)

constant:
 | TInt     { Int $1 }
 | TChar    { Char $1 }
 | TString  { String $1 }
 | TFloat   { Float $1 }

(*----------------------------*)
(* Labels *)
(*----------------------------*)

label_expr:
 | "~" label_ident
      { ArgImplicitTildeExpr ($1, Name $2) }
 | "?" label_ident
      { ArgImplicitQuestionExpr ($1, Name $2) }
 | TLabelDecl simple_expr %prec below_SHARP
      { ArgLabelTilde (Name $1 (* TODO remove the ~ and : *), $2) }
 | TOptLabelDecl simple_expr %prec below_SHARP
      { ArgLabelQuestion (Name $1 (* TODO remove the ~ and : *), $2) }

(*----------------------------*)
(* objects *)
(*----------------------------*)

field_expr_list:
 |  label "=" expr
      { }
  | field_expr_list ";" label "=" expr
      { }


(*************************************************************************)
(* Patterns *)
(*************************************************************************)

match_cases:
 | pattern  match_action                     { [Left ($1, $2)] }
 | match_cases "|"    pattern match_action { $1 @ [Right $2; Left ($3, $4)] }

match_action:
 | "->" seq_expr                  { Action ($1, $2) }
 | Twhen seq_expr "->" seq_expr   { WhenAction ($1, $2, $3, $4) }


pattern:
 | simple_pattern
      { $1 }

 | constr_longident pattern %prec prec_constr_appl
      { PatConstr ($1, Some $2) }
 | pattern_comma_list  %prec below_COMMA
      { PatTuple ($1) }
 | pattern "::" pattern
      { PatConsInfix ($1, $2, $3) }

 | pattern Tas val_ident
      { PatAs ($1, $2, Name $3) }

 (* nested patterns *)
 | pattern "|" pattern
      { PatDisj ($1, $2, $3) }

 (* name tag extension *)
 | name_tag pattern %prec prec_constr_appl
      { PatTodo }




simple_pattern:
 | val_ident %prec below_EQUAL
      { PatVar (Name $1) }
 | constr_longident
      { PatConstr ($1, None) }
 | "_"
      { PatUnderscore $1 }
 | signed_constant
      { PatConstant $1 }

 | "{" lbl_pattern_list record_pattern_end "}"
      { PatRecord ($1, $2, (* $3 *) $4) }
 | "[" pattern_semi_list opt_semi4 "]"
      { PatList (($1, $2 @ $3, $4)) }

 | "[|" pattern_semi_list opt_semi "|]"
      { PatTodo }
 | "[|" "|]"
      { PatTodo }

 (* note that let (x:...) a =  will trigger this rule *)
 | "(" pattern ":" core_type ")"
      { PatTyped ($1, $2, $3, $4, $5) }

 (* name tag extension *)
 | name_tag
      { PatTodo }
 (* range extension *)
 | TChar ".." TChar  
    { PatTodo }

 | "(" pattern ")"
    { ParenPat ($1, $2, $3) }


lbl_pattern_list:
 | label_longident "=" pattern               {[Left (PatField ($1, $2, $3))] }
 | label_longident                           {[Left (PatImplicitField ($1))]  }
 | lbl_pattern_list ";"   label_longident "=" pattern 
     { $1 @ [Right $2; Left (PatField ($3, $4, $5))] }
 | lbl_pattern_list ";"   label_longident       
     { $1 @ [Right $2; Left (PatImplicitField ($3))] }


record_pattern_end:
 | opt_semi                                    { }
 (* new 3.12 feature! *)
 | ";" "_" opt_semi              { }


pattern_semi_list:
 | pattern                                     { [Left $1] }
 | pattern_semi_list ";" pattern        { $1 @[Right $2; Left $3] }

pattern_comma_list:
 | pattern_comma_list "," pattern            { $1 @ [Right $2; Left $3] }
 | pattern "," pattern                       { [Left $1; Right $2; Left $3] }


signed_constant:
 | constant       { C2 $1 }
 | TMinus TInt    { CMinus ($1, Int $2) }
 | TMinus TFloat  { CMinus ($1, Float $2) }
 | TPlus TInt     { CPlus ($1, Int $2) }
 | TPlus TFloat   { CPlus ($1, Float $2) }

(*************************************************************************)
(* Types *)
(*************************************************************************)

type_constraint:
 | ":" core_type           { }
 
 (* object cast extension *)
 | ":>" core_type    { }

(*----------------------------*)
(* Types definitions *)
(*----------------------------*)

type_declarations:
 | type_declaration                            { [Left $1] }
 | type_declarations Tand type_declaration     { $1 @ [Right $2; Left $3] }

type_declaration:
  type_parameters TLowerIdent type_kind (*TODO constraints*)
   { 
     match $3 with
     | None -> 
         TyAbstract ($1, Name $2)
     | Some (tok_eq, type_kind) ->
         TyDef ($1, Name $2, tok_eq, type_kind)
   }


type_kind:
 | (*empty*)
      { None }
 | "=" core_type
      { Some ($1, TyCore $2) }
 | "=" constructor_declarations
      { Some ($1, TyAlgebric $2) }
 | "=" (*TODO private_flag*) "|" constructor_declarations
      { Some ($1, TyAlgebric (Right $2::$3)) }
 | "=" (*TODO private_flag*) "{" label_declarations opt_semi2 "}"
      { Some ($1, TyRecord ($2, ($3 @ $4), $5)) }



constructor_declarations:
 | constructor_declaration                     { [Left $1] }
 | constructor_declarations "|" constructor_declaration 
     { $1 @ [Right $2; Left $3] }

constructor_declaration:
    constr_ident constructor_arguments          { Name $1, $2 }

constructor_arguments:
 | (*empty*)                            { NoConstrArg }
 | Tof core_type_list                       { Of ($1, $2) }


type_parameters:
 |  (*empty*)                              { TyNoParam  }
 | type_parameter                              { TyParam1 $1 }
 | "(" type_parameter_list ")"         { TyParamMulti (($1, $2, $3)) }

type_parameter_list:
 | type_parameter                               { [Left $1] }
 | type_parameter_list "," type_parameter    { $1 @ [Right $2; Left $3] }

type_parameter:
  (*TODO type_variance*) "'" ident   { ($1, Name $2) }



label_declarations:
 | label_declaration                           { [Left $1] }
 | label_declarations ";" label_declaration   { $1 @[Right $2; Left $3]}

label_declaration:
  mutable_flag label ":" poly_type          
   { 
     {
       fld_mutable = $1;
       fld_name = Name $2;
       fld_tok = $3;
       fld_type = $4;
     }
   }

mutable_flag:
 | (*empty*)       { None }
 | Tmutable            { Some $1 }


(*----------------------------*)
(* Types expressions *)
(*----------------------------*)

core_type:
 | core_type2
    { $1 }

core_type2:
 | simple_core_type_or_tuple
     { $1 }
 | core_type2 "->" core_type2
     { TyFunction ($1, $2, $3) }

 (* ext: olabl *)
 | TLowerIdent           ":" core_type2 "->" core_type2
     { TyFunction ($3, $4, $5) (* TODO $1 $2 *)  }
 | "?" TLowerIdent ":" core_type2 "->" core_type2
     { TyFunction ($4, $5, $6) (* TODO $1 $2 *)  }
 (* pad: only because of lexer hack around labels *)
 | TOptLabelDecl                core_type2 "->" core_type2
     { TyFunction ($2, $3, $4) (* TODO $1 $2 *)  }


simple_core_type_or_tuple:
 | simple_core_type                          { $1 }
 | simple_core_type "*" core_type_list     { TyTuple (Left $1::Right $2::$3) }


simple_core_type:
 | simple_core_type2  %prec below_SHARP
      { $1 }
 (* weird diff between 'Foo of a * b' and 'Foo of (a * b)' *)
 | "(" core_type_comma_list ")" %prec below_SHARP
      { TyTuple2 (($1, $2, $3)) }

simple_core_type2:
 | "'" ident
      { TyVar ($1, Name $2) }
 | type_longident
      { TyName ($1) }
 | simple_core_type2 type_longident
      { TyApp (TyArg1 $1, $2) }
 | "(" core_type_comma_list ")" type_longident
      { TyApp (TyArgMulti (($1, $2, $3)), $4) }

 (* name tag extension *)
 | "[" row_field "|" row_field_list "]"
      { TyTodo }
 | "[" "|" row_field_list "]"
      { TyTodo }
 | "[" tag_field "]"
      { TyTodo }

 (* objects types *)
  | TLess meth_list TGreater
      { TyTodo }
  | TLess TGreater
      { TyTodo }


core_type_comma_list:
 | core_type                                  { [Left $1] }
 | core_type_comma_list "," core_type      { $1 @ [Right $2; Left $3] }

core_type_list:
  | simple_core_type                         { [Left $1] }
  | core_type_list "*" simple_core_type    { $1 @ [Right $2; Left $3] }

meth_list:
  | field ";" meth_list                      { }
  | field opt_semi                              {  }
  | ".."                                      {  }

field:
    label ":" poly_type             { }

(*----------------------------*)
(* Misc *)
(*----------------------------*)

poly_type:
 | core_type
     { $1 }


row_field_list:
 | row_field                                   { }
 | row_field_list "|" row_field                { }

row_field:
 | tag_field                                   { }
 | simple_core_type2                           { }

tag_field:
 | name_tag Tof opt_ampersand amper_type_list
      { }
 | name_tag
      { }

opt_ampersand:
 | TAnd                                   { }
 | (* empty *)                                 { }

amper_type_list:
 | core_type                                   { }
 | amper_type_list TAnd core_type         { }



(*************************************************************************)
(* Let/Fun definitions *)
(*************************************************************************)

let_bindings:
 | let_binding                           { [Left $1] }
 | let_bindings Tand let_binding         { $1 @ [Right $2; Left $3] }


let_binding:
 | val_ident fun_binding
      { 
        let (params, (teq, body)) = $2 in
        LetClassic {
          l_name = Name $1;
          l_params = params;
          l_tok = teq;
          l_body = body;
        }
      }
 | pattern "=" seq_expr
      { LetPattern ($1, $2, $3) }


fun_binding:
 | strict_binding { $1 }
 (* let x arg1 arg2 : t = e *)
 | type_constraint "=" seq_expr { [], ($2, $3) (* TODO return triple with $1*)}

strict_binding:
 (* simple values, e.g. 'let x = 1' *)
 | "=" seq_expr  { [], ($1, $2) }
 (* function values, e.g. 'let x a b c = 1' *)
 | labeled_simple_pattern fun_binding { let (args, body) = $2 in $1::args, body }

fun_def:
 | match_action                    { [], $1 }
 | labeled_simple_pattern fun_def  { let (args, body) = $2 in $1::args, body }


labeled_simple_pattern:
  | simple_pattern { ParamPat $1 }
  | label_pattern  { $1 }

opt_default:
 | (*empty*)           { None  }
 | "=" seq_expr            { Some ($1, $2) }

rec_flag:
 | (*empty*)   { None }
 | Trec            { Some $1 }

(*----------------------------*)
(* Labels *)
(*----------------------------*)

label_pattern:
  | "~" label_var
      { ParamTodo }
  (* ex: let x ~foo:a *)
  | TLabelDecl simple_pattern
      { ParamTodo }
  | "~" "(" label_let_pattern ")"
      { ParamTodo }
  | "?" "(" label_let_pattern opt_default ")"
      { ParamTodo }
  | "?" label_var
      { ParamTodo }

label_let_pattern:
 | label_var                   { }
 | label_var ":" core_type  { }

(*************************************************************************)
(* Classes *)
(*************************************************************************)

(*----------------------------*)
(* Class types *)
(*----------------------------*)
class_description:
 virtual_flag class_type_parameters TLowerIdent ":" class_type
  { }

class_type_declaration:
  virtual_flag class_type_parameters TLowerIdent "=" class_signature
  { }

class_type:
  | class_signature { }
  | simple_core_type_or_tuple "->" class_type { }

class_signature:
  (*  LBRACKET core_type_comma_list RBRACKET clty_longident
      {  }
  *)
  | clty_longident
      {  }
  | Tobject class_sig_body Tend
      {  }

class_sig_body:
    class_self_type class_sig_fields { }

class_self_type:
    "(" core_type ")"
      { }
  | (*empty*) {  }

class_sig_fields:
  | class_sig_fields Tinherit class_signature    {  }
  | class_sig_fields virtual_method_type        {  }
  | class_sig_fields method_type                {  }

  | class_sig_fields Tval value_type            {  }
(*
  | class_sig_fields Tconstraint constrain       {  }
*)
  | (*empty*)                               { }

method_type:
  | Tmethod private_flag label ":" poly_type { }

virtual_method_type:
  | Tmethod Tprivate Tvirtual label ":" poly_type
      {  }
  | Tmethod Tvirtual private_flag label ":" poly_type
      {  }

value_type:
  | Tvirtual mutable_flag label ":" core_type
      { }
  | Tmutable virtual_flag label ":" core_type
      {  }
  | label ":" core_type
      {  }

(*----------------------------*)
(* Class expressions *)
(*----------------------------*)

(*----------------------------*)
(* Class definitions *)
(*----------------------------*)

class_declaration:
    virtual_flag class_type_parameters TLowerIdent class_fun_binding
      { }

class_type_parameters:
  | (*empty*)                                   { }
  | "[" type_parameter_list "]"       { }

class_fun_binding:
  | "=" class_expr
      { }
  | labeled_simple_pattern class_fun_binding
      { }

class_expr:
  | class_simple_expr
      { }
  | Tfun class_fun_def
      { }
  | class_simple_expr simple_labeled_expr_list
      { }
  | Tlet rec_flag let_bindings Tin class_expr
      { }

class_simple_expr:
  | "[" core_type_comma_list "]" class_longident
      { }
  | class_longident
      { }
  | Tobject class_structure Tend
      { }
(* TODO
  | "(" class_expr ":" class_type ")"
      { }
*)
  | "(" class_expr ")"
      { }

class_fun_def:
  | labeled_simple_pattern "->" class_expr
      { }
  | labeled_simple_pattern class_fun_def
      { }

class_structure:
    class_self_pattern class_fields
      { }

class_self_pattern:
  | "(" pattern ")"
      { }
  | "(" pattern ":" core_type ")"
      { }
  | (*empty*)
      { }



class_fields:
  | (*empty*)
      { }
  | class_fields Tinherit override_flag class_expr parent_binder
      { }
  | class_fields Tval virtual_value
      { }
  | class_fields Tval value
      { }
  | class_fields virtual_method
      { }
  | class_fields concrete_method
      { }
(* TODO
  | class_fields Tconstraint constrain
      { }
*)
  | class_fields Tinitializer seq_expr
      { }


parent_binder:
  | Tas TLowerIdent
          { }
  | (* empty *)
          { }


virtual_value:
  | override_flag Tmutable Tvirtual label ":" core_type
      { }
  | Tvirtual mutable_flag label ":" core_type
      { }

value:
  | override_flag mutable_flag label "=" seq_expr
      { }
  | override_flag mutable_flag label type_constraint "=" seq_expr
      { }

virtual_method:
  | Tmethod override_flag Tprivate Tvirtual label ":" poly_type
      { }
  | Tmethod override_flag Tvirtual private_flag label ":" poly_type
      { }

concrete_method:
  | Tmethod override_flag private_flag label strict_binding
      { }
  | Tmethod override_flag private_flag label ":" poly_type "=" seq_expr
      { }



virtual_flag:
 | (* empty*)                               { }
 | Tvirtual                                     { }

(* 3.12? *)
override_flag:
 | (*empty*)                                 { }
 | "!"                                        { }

private_flag:
    (* empty *)                                 { }
  | Tprivate                                     { }

(*************************************************************************)
(* Modules *)
(*************************************************************************)

module_binding:
 | "=" module_expr
     { Some ($1, $2) }
 | "(" TUpperIdent ":" module_type ")" module_binding
     { None }
 | ":" module_type "=" module_expr
     { (* TODO $1 *) Some ($3, $4) }

module_declaration:
 | ":" module_type
      { }
 | "(" TUpperIdent ":" module_type ")" module_declaration
     { }

(*----------------------------*)
(* Module types *)
(*----------------------------*)

module_type:
 | mty_longident
      { }
 | Tsig signature Tend
      { }
 | Tfunctor "(" TUpperIdent ":" module_type ")" "->" module_type
      %prec below_WITH
      { }
 | module_type Twith with_constraints
     { }
 | "(" module_type ")"
      { }


with_constraint:
 | Ttype type_parameters label_longident with_type_binder core_type 
    (*constraints*)
   { }

with_type_binder:
 | "="          {  }
 | "=" Tprivate  {  }

(*----------------------------*)
(* Module expressions *)
(*----------------------------*)

module_expr:
  (* when just do a module aliasing *)
  | mod_longident
      { ModuleName $1 }
  (* nested modules *)
  | Tstruct structure Tend
      { ModuleStruct ($1, to_item $2, $3) }
  (* functor definition *)
  | Tfunctor "(" TUpperIdent ":" module_type ")" "->" module_expr
      { ModuleTodo }
  (* module/functor application *)
  | module_expr "(" module_expr ")"
      { ModuleTodo }

(*************************************************************************)
(* Attributes *)
(*************************************************************************)

(*pad: this is a limited implemen for now; just what I need for efuns *)

single_attr_id:
  | TLowerIdent { $1 }
  | TUpperIdent { $1 }
(* should also put all keywords here, but bad practice no? *)

attr_id:
  | single_attr_id {  }
  | single_attr_id "." attr_id { }

post_item_attribute:
  TBracketAtAt attr_id payload "]" { }

(* in theory you can have a full structure here *)
payload:
  | (* empty*) { }
  | TString { }


(*************************************************************************)
(* xxx_opt, xxx_list *)
(*************************************************************************)

opt_semi:
 | (*empty*)    { [] }
 | ";"       { [Right $1] }

opt_semi2:
 | (*empty*)    { [] }
 | ";"       { [Right $1] }

opt_semi3:
 | (*empty*)    { [] }
 | ";"       { [Right $1] }

opt_semi4:
 | (*empty*)    { [] }
 | ";"       { [Right $1] }

opt_bar:
 | (*empty*)    { [] }
 | "|"            { [Right $1] }

with_constraints:
 | with_constraint                             { [Left $1] }
 | with_constraints Tand with_constraint        { $1 @ [Right $2; Left $3] }

class_declarations:
  | class_declarations TAnd class_declaration   { }
  | class_declaration                           { }

class_descriptions:
  | class_descriptions TAnd class_description   { }
  | class_description                           { }

class_type_declarations:
  | class_type_declarations TAnd class_type_declaration  {  }
  | class_type_declaration                               { }

post_item_attributes:
  | (*empty*)  { [] }
  | post_item_attribute post_item_attributes { $1 :: $2 }
