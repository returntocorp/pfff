(* Joust: a Java lexer, parser, and pretty-printer written in OCaml
 * Copyright (C) 2001  Eric C. Cooper <ecc@cmu.edu>
 * Released under the GNU General Public License
 *
 * LALR(1) (ocamlyacc) grammar for Java
 *
 * Attempts to conform to:
 * The Java Language Specification, Second Edition
 * - James Gosling, Bill Joy, Guy Steele, Gilad Bracha
 *
 * Many modifications by Yoann Padioleau. Attempts to conform to:
 * The Java Language Specification, Third Edition, with some fixes from
 * http://www.cmis.brighton.ac.uk/staff/rnb/bosware/javaSyntax/syntaxV2.html
 * (broken link)
 *
 * Official (but incomplete) specification as of Java 14:
 * https://docs.oracle.com/javase/specs/jls/se14/html/jls-19.html
 *
 * More modifications by Yoann Padioleau to support more recent versions.
 * Copyright (C) 2011 Facebook
 * Copyright (C) 2020 r2c
 *
 * Support for:
 *  - generics (partial)
 *  - enums, foreach, ...
 *  - annotations (partial)
 *  - lambdas
 *)
%{
open Common
open AST_generic (* for the arithmetic operator *)
open Ast_java
module G = AST_generic

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* todo? use a Ast.special? *)
let this_ident ii = [], ("this", ii)
let super_ident ii = [], ("super", ii)
let super_identifier ii = ("super", ii)

let named_type (str, ii) = TBasic (str,ii)
let void_type ii = named_type ("void", ii)

(* we have to use a 'name' to specify reference types in the grammar
 * because of some ambiguity but what we really wanted was an
 * identifier followed by some type arguments.
 *)
let (class_type: name_or_class_type -> class_type) = fun xs ->
  xs |> List.map (function
  | Id x -> x, []
  | Id_then_TypeArgs (x, xs) -> x, xs
  | TypeArgs_then_Id _ -> raise Parsing.Parse_error
  )

let (name: name_or_class_type -> name) = fun xs ->
  xs |> List.map (function
  | Id x -> [], x
  | Id_then_TypeArgs (x, xs) ->
      (* this is ok because of the ugly trick we do for Cast
       * where we transform a Name into a ref_type
       *)
      xs, x
  | TypeArgs_then_Id (xs, Id x) ->
      xs, x
  | TypeArgs_then_Id (_xs, _) ->
      raise Parsing.Parse_error
  )

let (qualified_ident: name_or_class_type -> qualified_ident) = fun xs ->
  xs |> List.map (function
  | Id x -> x
  | Id_then_TypeArgs _ -> raise Parsing.Parse_error
  | TypeArgs_then_Id _ -> raise Parsing.Parse_error
  )


let expr_to_typename expr =
    match expr with
    | Name name ->
        TClass (name |> List.map (fun (xs, id) -> id, xs))
    (* ugly, undo what was done in postfix_expression *)
    | Dot (Name name, _, id) ->
        TClass ((name @ [[], id]) |> List.map (fun (xs, id) -> id, xs))
    | _ ->
        pr2 "cast_expression pb";
        pr2_gen expr;
        raise Todo

let mk_stmt_or_stmts = function
  | [] -> AStmts []
  | [x] -> AStmt x
  | xs -> AStmts xs
%}

(*************************************************************************)
(* Tokens *)
(*************************************************************************)

(* classic *)
%token <Parse_info.t> TUnknown
%token <Parse_info.t> EOF

(*-----------------------------------------*)
(* The comment tokens *)
(*-----------------------------------------*)
(* Those tokens are not even used in this file because they are
 * filtered in some intermediate phases (in Parse_java.lexer_function
 * by using TH.is_comment(). But they still must be declared
 * because ocamllex may generate them, or some intermediate phases may also
 * generate them (like some functions in parsing_hacks.ml).
 *)
%token <Parse_info.t> TComment TCommentNewline TCommentSpace

(*-----------------------------------------*)
(* The normal tokens *)
(*-----------------------------------------*)

(* tokens with "values" *)
%token <string * Parse_info.t> TInt TFloat TChar TString

%token <(string * Parse_info.t)> IDENTIFIER
%token <(string * Parse_info.t)> PRIMITIVE_TYPE

%token <Parse_info.t> LP "("		(* ( *)
%token <Parse_info.t> RP ")"		(* ) *)
%token <Parse_info.t> LC "{"		(* { *)
%token <Parse_info.t> RC "}"		(* } *)
%token <Parse_info.t> LB "["		(* [ *)
%token <Parse_info.t> RB "]"		(* ] *)
%token <Parse_info.t> SM ";"		(* ; *)
%token <Parse_info.t> CM ","		(* , *)
%token <Parse_info.t> DOT "."		(* . *)

%token <Parse_info.t> EQ "="		(* = *)
%token <Parse_info.t> GT		(* > *)
%token <Parse_info.t> LT		(* < *)
%token <Parse_info.t> NOT		(* ! *)
%token <Parse_info.t> COMPL		(* ~ *)
%token <Parse_info.t> COND		(* ? *)
%token <Parse_info.t> COLON ":"		(* : *)
%token <Parse_info.t> EQ_EQ		(* == *)
%token <Parse_info.t> LE		(* <= *)
%token <Parse_info.t> GE		(* >= *)
%token <Parse_info.t> NOT_EQ		(* != *)
%token <Parse_info.t> AND_AND		(* && *)
%token <Parse_info.t> OR_OR		(* || *)
%token <Parse_info.t> INCR		(* ++ *)
%token <Parse_info.t> DECR		(* -- *)
%token <Parse_info.t> PLUS		(* + *)
%token <Parse_info.t> MINUS		(* - *)
%token <Parse_info.t> TIMES		(* * *)
%token <Parse_info.t> DIV		(* / *)
%token <Parse_info.t> AND		(* & *)
%token <Parse_info.t> OR		(* | *)
%token <Parse_info.t> XOR		(* ^ *)
%token <Parse_info.t> MOD		(* % *)
%token <Parse_info.t> LS		(* << *)
%token <Parse_info.t> SRS		(* >> *)
%token <Parse_info.t> URS		(* >>> *)

%token <Parse_info.t> AT "@"		(* @ *)
%token <Parse_info.t> DOTS "..."		(* ... *) LDots "<..." RDots "...>"
%token <Parse_info.t> ARROW "->"		(* -> *)
%token <Parse_info.t> COLONCOLON "::"		(* :: *)


%token <(AST_generic.operator * Parse_info.t)> OPERATOR_EQ
	(* += -= *= /= &= |= ^= %= <<= >>= >>>= *)

(* keywords tokens *)
%token <Parse_info.t>
 ABSTRACT BREAK CASE CATCH CLASS CONST CONTINUE
 DEFAULT DO ELSE EXTENDS FINAL FINALLY FOR GOTO
 IF IMPLEMENTS IMPORT INSTANCEOF INTERFACE
 NATIVE NEW PACKAGE PRIVATE PROTECTED PUBLIC RETURN
 STATIC STRICTFP SUPER SWITCH SYNCHRONIZED
 THIS THROW THROWS TRANSIENT TRY VOID VOLATILE WHILE
 (* javaext: *)
 ASSERT
 ENUM
 TRUE FALSE NULL
 VAR

(*-----------------------------------------*)
(* Extra tokens: *)
(*-----------------------------------------*)

(* to avoid some conflicts *)
%token <Parse_info.t> LB_RB

(* Those fresh tokens are created in parsing_hacks_java.ml *)
%token <Parse_info.t> LT_GENERIC		(* < ... > *)
%token <Parse_info.t> LP_LAMBDA		(* ( ... ) ->  *)
%token <Parse_info.t> DEFAULT_COLON		(* default :  *)

(*************************************************************************)
(* Priorities *)
(*************************************************************************)

(*************************************************************************)
(* Rules type declaration *)
(*************************************************************************)
(*
 * The start production must begin with a lowercase letter,
 * because ocamlyacc defines the parsing function with that name.
 *)
%start goal sgrep_spatch_pattern
%type <Ast_java.program> goal
%type <Ast_java.any>     sgrep_spatch_pattern

%%
(*************************************************************************)
(* Macros *)
(*************************************************************************)
optl(X):
 | (* empty *) { [] }
 | X           { $1 }

(*************************************************************************)
(* TOC *)
(*************************************************************************)
(* TOC:
 *  goal
 *  name
 *  type
 *  expr
 *  statement
 *  declaration
 *  anotation
 *  class/interfaces
 *)

(*************************************************************************)
(* Toplevel *)
(*************************************************************************)

goal: compilation_unit EOF  { $1 }

(* conflicts: was simply 
 *  package_declaration_opt import_declarations_opt type_declarations_opt
 * but with an annotation now possible on package_declaration, seeing an
 * '@' the LALR(1) parser does not know if it's the start of an annotation
 * for a package or class_declaration. So we need to unfold those _opt.
 *)
compilation_unit:
  | package_declaration import_declaration+ type_declaration*
    { [DirectiveStmt $1] @ ($2 |> List.map (fun x -> DirectiveStmt x)) @ List.flatten $3 }
  | package_declaration                     type_declaration*
    { [DirectiveStmt $1] @ List.flatten $2 }
  |                     import_declaration+ type_declaration*
    { ($1 |> List.map (fun x -> DirectiveStmt x)) @ List.flatten $2 }
  |                                         type_declaration*
    { List.flatten $1 }

declaration:
 | class_declaration      { Class $1 }
 | interface_declaration  { Class $1 }
 | enum_declaration       { Enum $1 }
 | method_declaration     { Method $1 }

sgrep_spatch_pattern:
 | import_declaration EOF           { AStmt (DirectiveStmt $1) }
 | import_declaration import_declaration+ EOF  
    { AStmts (($1::$2) |> List.map (fun x -> DirectiveStmt x)) }
 | package_declaration EOF          { AStmt (DirectiveStmt $1) }
 | expression EOF                   { AExpr $1 }
 | item_no_dots EOF                 { mk_stmt_or_stmts $1 }
 | item_no_dots item_sgrep_list EOF { mk_stmt_or_stmts ($1 @ (List.flatten $2)) }

item_no_dots:
 | statement_no_dots { [$1] }
 | declaration       { [DeclStmt $1] }
 | local_variable_declaration_statement { $1 }

(* coupling: copy paste of statement, without dots *)
statement_no_dots:
 | statement_without_trailing_substatement  { $1 }
 | labeled_statement  { $1 }
 | if_then_statement  { $1 }
 | if_then_else_statement  { $1 }
 | while_statement  { $1 }
 | for_statement  { $1 }

item_sgrep:
 | statement { [$1] }
 | declaration { [DeclStmt $1] }
 | local_variable_declaration_statement { $1 }

item_sgrep_list:
 | item_sgrep { [$1] }
 | item_sgrep_list item_sgrep { $1 @ [$2] }


(*************************************************************************)
(* Package, Import, Type *)
(*************************************************************************)

(* ident_list *)
package_declaration: modifiers_opt PACKAGE qualified_ident ";"  
  { Package ($2, $3, $4) (* TODO $1*)}

(* javaext: static_opt 1.? *)
import_declaration:
 | IMPORT STATIC? name ";"            
    { (Import ($2, 
      (match List.rev (qualified_ident $3) with
      | x::xs -> ImportFrom ($1, List.rev xs, x)
      | [] -> raise Impossible
      ))) }
 | IMPORT STATIC? name "." TIMES ";"  
    { (Import ($2, ImportAll ($1, qualified_ident $3, $5)))}

type_declaration:
 | class_declaration      { [DeclStmt (Class $1)] }
 | interface_declaration  { [DeclStmt (Class $1)] }
 | ";"  { [] }

 (* javaext: 1.? *)
 | enum_declaration            { [DeclStmt (Enum $1)] }
 | annotation_type_declaration { [DeclStmt (Class $1)] }

(*************************************************************************)
(* Ident, namespace  *)
(*************************************************************************)
identifier: IDENTIFIER { $1 }

qualified_ident: 
  | IDENTIFIER                     { [$1] }
  | qualified_ident "." IDENTIFIER { $1 @ [$3] }

name:
 | identifier_           { [$1] }
 | name "." identifier_  { $1 @ [$3] }
 | name "." LT_GENERIC type_arguments_args_opt GT identifier_ 
     { $1@[TypeArgs_then_Id($4,$6)] }

identifier_:
 | identifier                       { Id $1 }
 | identifier LT_GENERIC type_arguments_args_opt GT { Id_then_TypeArgs($1, $3) }

(*************************************************************************)
(* Types *)
(*************************************************************************)

type_:
 | primitive_type  { $1 }
 | reference_type  { $1 }

primitive_type: PRIMITIVE_TYPE  { named_type $1 }

class_or_interface_type: name { TClass (class_type $1) }

reference_type:
 | class_or_interface_type { $1 }
 | array_type { $1 }

array_type:
 | primitive_type          LB_RB { TArray $1 }
 | class_or_interface_type (* was name *) LB_RB { TArray $1 }
 | array_type              LB_RB { TArray $1 }

(*----------------------------*)
(* Generics arguments *)
(*----------------------------*)

(* javaext: 1? *)
type_argument:
 | reference_type { TArgument $1 }
 | COND           { TQuestion None }
 | COND EXTENDS reference_type { TQuestion (Some (false, $3)) }
 | COND SUPER   reference_type { TQuestion (Some (true, $3))}

(*----------------------------*)
(* Generics parameters *)
(*----------------------------*)
(* javaext: 1? *)
type_parameters:
 | LT type_parameters_bis GT { $2 }

type_parameter:
 | identifier               { TParam ($1, []) }
 | identifier EXTENDS bound { TParam ($1, $3) }

bound: ref_type_and_list { $1 }

(*************************************************************************)
(* Expressions *)
(*************************************************************************)

typed_metavar:
 | "(" type_ IDENTIFIER ")" { Flag_parsing.sgrep_guard (TypedMetavar($3, $2))  }

primary:
 | primary_no_new_array       { $1 }
 | array_creation_expression  { $1 }

primary_no_new_array:
 | literal             { $1 }
 | THIS                { Name [this_ident $1] }
 | "(" expression ")"    { $2 }
 | class_instance_creation_expression { $1 }
 | field_access                       { $1 }
 | method_invocation                  { $1 }
 | array_access                       { $1 }
 (* sgrep-ext: *)
 | typed_metavar       { $1 }
 (* javaext: ? *)
 | name "." THIS       { Name (name $1 @ [this_ident $3]) }
 (* javaext: ? *)
 | class_literal       { $1 }
 (* javaext: ? *)
 | method_reference { $1 }
 (* javaext: ? *)
 | array_creation_expression_with_initializer { $1 }

literal:
 | TRUE   { Literal (Bool (true, $1)) }
 | FALSE   { Literal (Bool (false, $1)) }
 | TInt    { Literal (Int ($1)) }
 | TFloat  { Literal (Float ($1)) }
 | TChar   { Literal (Char ($1)) }
 | TString { Literal (String ($1)) }
 | NULL   { Literal (Null $1) }

class_literal:
 | primitive_type "." CLASS  { ClassLiteral $1 }
 | name           "." CLASS  { ClassLiteral (TClass (class_type ($1))) }
 | array_type     "." CLASS  { ClassLiteral $1 }
 | VOID           "." CLASS  { ClassLiteral (void_type $1) }

class_instance_creation_expression:
 | NEW name "(" argument_list_opt ")" 
   class_body?
   { NewClass ($1, TClass (class_type $2), ($3,$4,$5), $6) }
 (* javaext: ? *)
 | primary "." NEW identifier "(" argument_list_opt ")" class_body?
   { NewQualifiedClass ($1, $2, $3, TClass ([$4,[]]), ($5,$6,$7), $8) }
 (* javaext: not in 2nd edition java language specification. *)
 | name "." NEW identifier "(" argument_list_opt ")" class_body?
   { NewQualifiedClass ((Name (name $1)), $2, $3, TClass [$4,[]],($5,$6,$7),$8)}

(*
   A new array that cannot be accessed right away by appending [index]:
    new String[2][1]  // a 2-dimensional array
*)
array_creation_expression:
 | NEW primitive_type dim_expr+ dims_opt
       { NewArray ($1, $2, $3, $4, None) }
 | NEW name dim_expr+ dims_opt
       { NewArray ($1, TClass (class_type ($2)), $3, $4, None) }

(*
   A new array that can be accessed right away by appending [index] as follows:
    new String[] { "abc", "def" }[1]  // a string
*)
array_creation_expression_with_initializer:
 | NEW primitive_type dims array_initializer
       { NewArray ($1, $2, [], $3, Some $4) }
 | NEW name dims array_initializer
       { NewArray ($1, TClass (class_type ($2)), [], $3, Some $4) }

dim_expr: "[" expression "]"  { $2 }

dims:
 |      LB_RB  { 1 }
 | dims LB_RB  { $1 + 1 }

field_access:
 | primary "." identifier        { Dot ($1, $2, $3) }
 | SUPER   "." identifier        { Dot (Name [super_ident $1], $2, $3) }
 (* javaext: ? *)
 | name "." SUPER "." identifier { Dot (Name (name $1@[super_ident $3]),$2,$5)}

array_access:
 | name "[" expression "]"                  { ArrayAccess ((Name (name $1)), $3)}
 | primary_no_new_array "[" expression "]"  { ArrayAccess ($1, $3) }

(*----------------------------*)
(* Method call *)
(*----------------------------*)

method_invocation:
 | name "(" argument_list_opt ")"
        { match List.rev $1 with
          (* TODO: lose information of TypeArgs_then_Id *)
          | ((Id x) | (TypeArgs_then_Id (_, Id x)))::xs ->
              let (xs: identifier_ list) =
                (match xs with
                (* should be a "this" or "self" *)
                | [] -> [Id ("this", snd x)]
                | _ -> List.rev xs
                )
              in
              Call (Dot (Name (name (xs)), Parse_info.fake_info ".", x), 
                     ($2,$3,$4))
          | _ ->
              pr2 "method_invocation pb";
              pr2_gen $1;
              raise Impossible
        }
 | primary "." identifier "(" argument_list_opt ")"
	{ Call ((Dot ($1, $2, $3)), ($4,$5,$6)) }
 | SUPER "." identifier "(" argument_list_opt ")"
	{ Call ((Dot (Name [super_ident $1], $2, $3)), ($4,$5,$6)) }
 (* javaext: ? *)
 | name "." SUPER "." identifier "(" argument_list_opt ")"
	{ Call (Dot (Name (name $1 @ [super_ident $3]), $2, $5), ($6,$7,$8))}

argument: 
 | expression { $1 }

(*----------------------------*)
(* Arithmetic *)
(*----------------------------*)

postfix_expression: (* EJ todo maybe need to add typed metavars here *)
 | primary  { $1 }
 | name     {
     (* Ambiguity. It could be a field access (Dot) or a qualified
      * name (Name). See ast_java.ml note on the Dot constructor for
      * more information.
      * The last dot has to be a Dot and not a Name at least,
      * but more elements of Name could be a Dot too.
      *)
     match List.rev $1 with
     | (Id id)::x::xs ->
         Dot (Name (name (List.rev (x::xs))), Parse_info.fake_info ".", id)
     | _ ->
         Name (name $1)
   }

 | post_increment_expression  { $1 }
 | post_decrement_expression  { $1 }

post_increment_expression: postfix_expression INCR  
  { Postfix ($1, (AST_generic.Incr, $2)) }

post_decrement_expression: postfix_expression DECR  
  { Postfix ($1, (AST_generic.Decr, $2)) }

unary_expression:
 | pre_increment_expression  { $1 }
 | pre_decrement_expression  { $1 }
 | PLUS unary_expression  { Unary ((AST_generic.Plus,$1), $2) }
 | MINUS unary_expression  { Unary ((AST_generic.Minus,$1), $2) }
 | unary_expression_not_plus_minus  { $1 }

pre_increment_expression: INCR unary_expression  
  { Prefix ((AST_generic.Incr, $1), $2) }

pre_decrement_expression: DECR unary_expression  
  { Prefix ((AST_generic.Decr, $1), $2) }

(* see conflicts.txt Cast note to understand the need of this rule *)
unary_expression_not_plus_minus:
 | postfix_expression  { $1 }
 | COMPL unary_expression  { Unary ((AST_generic.BitNot,$1), $2) }
 | NOT unary_expression    { Unary ((AST_generic.Not,$1), $2) }
 | cast_expression  { $1 }

(* original rule:
 * | "(" primitive_type dims_opt ")" unary_expression
 * | "(" reference_type ")" unary_expression_not_plus_minus
 * Semantic action must ensure that '( expression )' is really '( name )'.
 * Conflict with regular paren expr; when see ')' dont know if
 * can reduce to expr or shift name, so have to use
 * expr in both cases (see primary_new_array for the other rule
 * using "(" expression ")")
 *)
cast_expression:
 | "(" primitive_type ")" unary_expression  { Cast (($1,[$2],$3), $4) }
 | "(" array_type ")" unary_expression_not_plus_minus  { Cast (($1,[$2],$3), $4) }
 | "(" expression ")" unary_expression_not_plus_minus
	{  Cast (($1,[expr_to_typename $2],$3), $4) }

cast_lambda_expression:
 (* this can not be put inside cast_expression. See conflicts.txt*)
 | "(" expression ")" lambda_expression 
     { Cast (($1,[expr_to_typename $2],$3), $4) }


multiplicative_expression:
 | unary_expression  { $1 }
 | multiplicative_expression TIMES unary_expression { Infix ($1, (Mult,$2) , $3) }
 | multiplicative_expression DIV unary_expression   { Infix ($1, (Div,$2), $3) }
 | multiplicative_expression MOD unary_expression   { Infix ($1, (Mod,$2), $3) }

additive_expression:
 | multiplicative_expression  { $1 }
 | additive_expression PLUS multiplicative_expression { Infix ($1, (Plus,$2), $3) }
 | additive_expression MINUS multiplicative_expression { Infix ($1, (Minus,$2), $3) }

shift_expression:
 | additive_expression  { $1 }
 | shift_expression LS additive_expression  { Infix ($1, (LSL,$2), $3) }
 | shift_expression SRS additive_expression  { Infix ($1, (LSR,$2), $3) }
 | shift_expression URS additive_expression  { Infix ($1, (ASR,$2), $3) }

relational_expression:
 | shift_expression  { $1 }
 (* possible many conflicts if don't use a LT2 *)
 | relational_expression LT shift_expression  { Infix ($1, (Lt,$2), $3) }
 | relational_expression GT shift_expression  { Infix ($1, (Gt,$2), $3) }
 | relational_expression LE shift_expression  { Infix ($1, (LtE,$2), $3) }
 | relational_expression GE shift_expression  { Infix ($1, (GtE,$2), $3) }
 | relational_expression INSTANCEOF reference_type  { InstanceOf ($1, $3) }

equality_expression:
 | relational_expression  { $1 }
 | equality_expression EQ_EQ relational_expression  { Infix ($1, (Eq,$2), $3) }
 | equality_expression NOT_EQ relational_expression { Infix ($1, (NotEq,$2), $3) }

and_expression:
 | equality_expression  { $1 }
 | and_expression AND equality_expression  { Infix ($1, (BitAnd,$2), $3) }

exclusive_or_expression:
 | and_expression  { $1 }
 | exclusive_or_expression XOR and_expression  { Infix ($1, (BitXor,$2), $3) }

inclusive_or_expression:
 | exclusive_or_expression  { $1 }
 | inclusive_or_expression OR exclusive_or_expression  { Infix ($1, (BitOr,$2), $3) }

conditional_and_expression:
 | inclusive_or_expression  { $1 }
 | conditional_and_expression AND_AND inclusive_or_expression
     { Infix($1,(And,$2),$3) }

conditional_or_expression:
 | conditional_and_expression  { $1 }
 | conditional_or_expression OR_OR conditional_and_expression
     { Infix ($1, (Or, $2), $3) }

(*----------------------------*)
(* Ternary *)
(*----------------------------*)

conditional_expression:
 | conditional_or_expression
     { $1 }
 | conditional_or_expression COND expression ":" conditional_expression
     { Conditional ($1, $3, $5) }
 | conditional_or_expression COND expression ":" lambda_expression
     { Conditional ($1, $3, $5) }

(*----------------------------*)
(* Assign *)
(*----------------------------*)

assignment_expression:
 | conditional_expression  { $1 }
 | assignment              { $1 }
 (* sgrep-ext: *)
 | "..." { Flag_parsing.sgrep_guard (Ellipsis $1) }
 | "<..." expression "...>" { Flag_parsing.sgrep_guard (DeepEllipsis ($1,$2,$3))}


(* javaext: was assignment_expression for rhs, but we want lambdas there*)
assignment: left_hand_side assignment_operator expression
    { $2 $1 $3 }


left_hand_side:
 | name          { Name (name $1) }
 | field_access  { $1 }
 | array_access  { $1 }
 (* sgrep-ext: *)
 | typed_metavar { $1 }

assignment_operator:
 | "="  { (fun e1 e2 -> Assign (e1, $1, e2))  }
 | OPERATOR_EQ  { (fun e1 e2 -> AssignOp (e1, $1, e2)) }

(*----------------------------*)
(* Lambdas *)
(*----------------------------*)
lambda_expression: lambda_parameters "->" lambda_body 
  { Lambda ($1, $3) }

lambda_parameters: 
 | IDENTIFIER { [mk_param_id $1] }
 | LP_LAMBDA lambda_parameter_list ")" { $2 }
 | LP_LAMBDA ")" { [] }

lambda_parameter_list: 
 | identifier_list { $1 |> List.map mk_param_id }
 | lambda_param_list { $1 }

identifier_list:
 | identifier  { [$1] }
 | identifier_list "," identifier  { $3 :: $1 }

lambda_param_list:
 | lambda_param  { [$1] }
 | lambda_param_list "," lambda_param  { $3 :: $1 }

lambda_param:
 | variable_modifier+ lambda_parameter_type variable_declarator_id 
    { ParamClassic (canon_var $1 $2 $3)  }
 |                    lambda_parameter_type variable_declarator_id 
    { ParamClassic (canon_var [] $1 $2) }
 | variable_arity_parameter { $1 }

lambda_parameter_type:
 | unann_type { Some $1 }
 | VAR        { None }

unann_type: type_ { $1 }

variable_arity_parameter: 
 | variable_modifier+ unann_type "..." identifier 
    { ParamClassic (canon_var $1 (Some $2) (IdentDecl $4)) }
 |                    unann_type "..." identifier 
    { ParamClassic (canon_var [] (Some $1) (IdentDecl $3)) }

(* no need %prec LOW_PRIORITY_RULE as in parser_js.mly ?*)
lambda_body:
 | expression { Expr ($1, G.sc) }
 | block      { $1 }

(*----------------------------*)
(* Method reference *)
(*----------------------------*)
(* javaext: ? TODO AST *)
(* reference_type is inlined because of classic ambiguity with name *)
method_reference: 
 | name       "::" identifier { Literal (Null $2) }
 | primary    "::" identifier { Literal (Null $2) }
 | array_type "::" identifier { Literal (Null $2) }
 | name       "::" NEW { Literal (Null $2) }
 | array_type "::" NEW { Literal (Null $2) }
 | SUPER      "::" identifier { Literal (Null $2) }
 | name "." SUPER   "::" identifier { Literal (Null $2) }

(*----------------------------*)
(* Shortcuts *)
(*----------------------------*)
expression: 
 | assignment_expression  { $1 }
 (* javaext: ? *)
 | lambda_expression { $1 }
 (* this can not be put inside cast_expression. See conflicts.txt*)
 | cast_lambda_expression { $1 }

constant_expression: expression  { $1 }

(*************************************************************************)
(* Statements *)
(*************************************************************************)

statement:
 | statement_without_trailing_substatement  { $1 }

 | labeled_statement  { $1 }
 | if_then_statement  { $1 }
 | if_then_else_statement  { $1 }
 | while_statement  { $1 }
 | for_statement  { $1 }
 (* sgrep-ext: *)
 | "..." { Flag_parsing.sgrep_guard (Expr (Ellipsis $1, G.sc)) }

statement_without_trailing_substatement:
 | block  { $1 }
 | empty_statement  { $1 }
 | expression_statement  { $1 }
 | switch_statement  { $1 }
 | do_statement  { $1 }
 | break_statement  { $1 }
 | continue_statement  { $1 }
 | return_statement  { $1 }
 | synchronized_statement  { $1 }
 | throw_statement  { $1 }
 | try_statement  { $1 }
 (* javaext:  *)
 | ASSERT expression ";"                  { Assert ($1, $2, None) }
 | ASSERT expression ":" expression ";" { Assert ($1, $2, Some $4) }

block: "{" block_statement* "}"  { Block ($1, List.flatten $2, $3) }

block_statement:
 | local_variable_declaration_statement  { $1 }
 | statement          { [$1] }
 (* javaext: ? *)
 | class_declaration  { [DeclStmt (Class $1)] }

local_variable_declaration_statement: local_variable_declaration ";"
 { List.map (fun x -> LocalVar x) $1 }

(* cant factorize with variable_modifier_opt, conflicts otherwise *)
local_variable_declaration: modifiers_opt type_ variable_declarators
 (* javaext: 1.? actually should be variable_modifiers but conflict *)
     { decls (fun x -> x) $1 $2 (List.rev $3) }

empty_statement: ";" { EmptyStmt $1 }

labeled_statement: identifier ":" statement
   { Label ($1, $3) }

expression_statement: statement_expression ";"  { Expr ($1, $2) }

(* pad: good *)
statement_expression:
 | assignment  { $1 }
 | pre_increment_expression  { $1 }
 | pre_decrement_expression  { $1 }
 | post_increment_expression  { $1 }
 | post_decrement_expression  { $1 }
 | method_invocation  { $1 }
 | class_instance_creation_expression  { $1 }
 (* sgrep-ext: to allow '$S;' in sgrep *)
 | IDENTIFIER { Flag_parsing.sgrep_guard ((Name (name [Id $1])))  }
 | typed_metavar { $1 }


if_then_statement: IF "(" expression ")" statement
   { If ($1, $3, $5, None) }

if_then_else_statement: IF "(" expression ")" statement_no_short_if ELSE statement
   { If ($1, $3, $5, Some $7) }


switch_statement: SWITCH "(" expression ")" switch_block
    { Switch ($1, $3, $5) }

switch_block:
 | "{"                                             "}"  { [] }
 | "{"                               switch_label+ "}"  { [$2, []] }
 | "{" switch_block_statement_groups               "}"  { $2 }
 | "{" switch_block_statement_groups switch_label+ "}"
     { List.rev (($3, []) :: $2) }

switch_block_statement_group: switch_label+ block_statement+
   {$1, List.flatten $2}

switch_label:
 | CASE constant_expression ":"  { Case ($1, $2) }
 | DEFAULT_COLON ":"                   { Default $1 }


while_statement: WHILE "(" expression ")" statement
     { While ($1, $3, $5) }

do_statement: DO statement WHILE "(" expression ")" ";"
     { Do ($1, $2, $5) }

(*----------------------------*)
(* For *)
(*----------------------------*)

for_statement:
  FOR "(" for_control ")" statement
	{ For ($1, $3, $5) }

for_control:
 | for_init_opt ";" expression? ";" optl(for_update)
     { ForClassic ($1, Common2.option_to_list $3, $5) }
 (* javeext: ? *)
 | for_var_control
     { let (a, b) = $1 in Foreach (a, b) }

for_init_opt:
 | (*empty*)  { ForInitExprs [] }
 | for_init       { $1 }

for_init:
| statement_expression_list   { ForInitExprs $1 }
| local_variable_declaration  { ForInitVars $1 }

for_update: statement_expression_list  { $1 }

for_var_control: 
 modifiers_opt type_ variable_declarator_id for_var_control_rest
  (* actually only FINAL is valid here, but cant because get shift/reduce
   * conflict otherwise because for_init can be a local_variable_decl
   *)
     { canon_var $1 (Some $2) $3, $4 }

for_var_control_rest: ":" expression { $2 }

(*----------------------------*)
(* Other *)
(*----------------------------*)

break_statement: BREAK identifier? ";"  { Break ($1, $2) }
continue_statement: CONTINUE identifier? ";"  { Continue ($1, $2) }
return_statement: RETURN expression? ";"  { Return ($1, $2) }

synchronized_statement: SYNCHRONIZED "(" expression ")" block { Sync ($3, $5) }

(*----------------------------*)
(* Exceptions *)
(*----------------------------*)

throw_statement: THROW expression ";"  { Throw ($1, $2) }

try_statement:
 | TRY block catch_clause+        { Try ($1, None, $2, $3, None) }
 | TRY block catch_clause* finally  { Try ($1, None, $2, $3, Some $4) }
 (* javaext: ? *)
 | TRY resource_specification block catch_clause* finally? { 
    Try ($1, Some $2, $3, $4, $5)
  }

finally: FINALLY block  { $1, $2 }

catch_clause:
 | CATCH "(" catch_formal_parameter ")" block  { $1, $3, $5 }
 (* javaext: not in 2nd edition java language specification.*)
 | CATCH "(" catch_formal_parameter ")" empty_statement  { $1, $3, $5 }

(* javaext: ? was just formal_parameter before *)
catch_formal_parameter: 
  | variable_modifier+ catch_type variable_declarator_id 
      { canon_var $1 (Some (fst $2)) $3, snd $2 }
  |                    catch_type variable_declarator_id 
      { canon_var [] (Some (fst $1)) $2, snd $1 }

(* javaext: ? *)
catch_type: catch_type_list { List.hd $1, List.tl $1 }

catch_type_list:
  | type_ { [$1] }
  | catch_type_list OR type_ { $1 @ [$3] }

(* javaext: ? *)
resource_specification: "(" resource_list ";"? ")" { $1, [](* TODO $2*), $4 }

resource: 
 | variable_modifier+ local_variable_type identifier "=" expression { }
 |                    local_variable_type identifier "=" expression { }
 | variable_access { }
 
local_variable_type: 
 | unann_type { }

variable_access:
 | field_access { }
 | name { }

(*----------------------------*)
(* No short if *)
(*----------------------------*)

statement_no_short_if:
 | statement_without_trailing_substatement  { $1 }
 | labeled_statement_no_short_if  { $1 }
 | if_then_else_statement_no_short_if  { $1 }
 | while_statement_no_short_if  { $1 }
 | for_statement_no_short_if  { $1 }

labeled_statement_no_short_if: identifier ":" statement_no_short_if
   { Label ($1, $3) }

if_then_else_statement_no_short_if:
 IF "(" expression ")" statement_no_short_if ELSE statement_no_short_if
   { If ($1, $3, $5, Some $7) }

while_statement_no_short_if: WHILE "(" expression ")" statement_no_short_if
     { While ($1, $3, $5) }

for_statement_no_short_if:
  FOR "(" for_control ")" statement_no_short_if
	{ For ($1, $3, $5) }

(*************************************************************************)
(* Modifiers *)
(*************************************************************************)

(*
 * to avoid shift/reduce conflicts, we accept all modifiers
 * in front of all declarations.  the ones not applicable to
 * a particular kind of declaration must be detected in semantic actions.
 *)
modifier:
 | PUBLIC       { Public, $1 }
 | PROTECTED    { Protected, $1 }
 | PRIVATE      { Private, $1 }

 | ABSTRACT     { Abstract, $1 }
 | STATIC       { Static, $1 }
 | FINAL        { Final, $1 }

 | STRICTFP     { StrictFP, $1 }
 | TRANSIENT    { Transient, $1 }
 | VOLATILE     { Volatile, $1 }
 | SYNCHRONIZED { Synchronized, $1 }
 | NATIVE       { Native, $1 }

 | DEFAULT      { DefaultModifier, $1 }

 | annotation { Annotation $1, (Common2.fst3 $1) }

(*************************************************************************)
(* Annotation *)
(*************************************************************************)

annotation:
 | "@" name { ($1, $2, None) }
 | "@" name "(" annotation_element ")" { ($1, $2, Some ($3, $4, $5)) }

annotation_element:
 | (* nothing *) { EmptyAnnotArg }
 | element_value { AnnotArgValue $1 }
 | element_value_pairs { AnnotArgPairInit $1 }

element_value:
 | expr1 { AnnotExprInit $1 }
 | annotation { AnnotNestedAnnot $1 }
 | element_value_array_initializer { AnnotArrayInit $1 }

element_value_pair:
 | identifier "=" element_value { ($1, $3) }


element_value_array_initializer:
 | "{" "}" { [] }
 | "{" element_values "}" { $2 }
 | "{" element_values "," "}" { $2 }

(* should be statically a constant expression; can contain '+', '*', etc.*)
expr1: 
 | conditional_expression { $1 }
 (* sgrep-et: *)
 | "..." { Flag_parsing.sgrep_guard (Ellipsis $1) }

(*************************************************************************)
(* Class *)
(*************************************************************************)

class_declaration:
 modifiers_opt CLASS identifier type_parameters_opt super? optl(interfaces)
 class_body
  { { cl_name = $3; cl_kind = ClassRegular;
      cl_mods = $1; cl_tparams = $4;
      cl_extends = $5;  cl_impls = $6;
      cl_body = $7;
     }
  }

super: EXTENDS type_ (* was class_type *)  { $2 }

interfaces: IMPLEMENTS ref_type_list (* was interface_type_list *)  { $2 }

(*----------------------------*)
(* Class body *)
(*----------------------------*)
class_body: "{" class_body_declaration* "}"  { $1, List.flatten $2, $3 }

class_body_declaration:
 | class_member_declaration  { $1 }
 | constructor_declaration  { [$1] }
 | static_initializer  { [$1] }
 (* javaext: 1.? *)
 | instance_initializer  { [$1] }
 
class_member_declaration:
 | field_declaration  { $1 }
 | method_declaration  { [Method $1] }

 (* javaext: 1.? *)
 | generic_method_or_constructor_decl { $1 }
 (* javaext: 1.? *)
 | class_declaration  { [Class $1] }
 | interface_declaration  { [Class $1] }
 (* javaext: 1.? *)
 | enum_declaration { [Enum $1] }
 (* javaext: 1.? *)
 | annotation_type_declaration { [Class $1] }

 | ";"  { [] }
 (* sgrep-ext: allows ... inside class body *)
 | "..." { [DeclEllipsis $1] }


static_initializer: STATIC block  { Init (Some $1, $2) }

instance_initializer: block       { Init (None, $1) }

(*----------------------------*)
(* Field *)
(*----------------------------*)

field_declaration: modifiers_opt type_ variable_declarators ";"
   { decls (fun x -> Field x) $1 $2 (List.rev $3) }

variable_declarator:
 | variable_declarator_id  { $1, None }
 | variable_declarator_id "=" variable_initializer  { $1, Some $3 }

variable_declarator_id:
 | identifier                    { IdentDecl $1 }
 | variable_declarator_id LB_RB  { ArrayDecl $1 }

variable_initializer:
 | expression         { ExprInit $1 }
 | array_initializer  { $1 }

array_initializer:
 | "{" ","? "}"                        { ArrayInit ($1, [], $3) }
 | "{" variable_initializers ","? "}"  { ArrayInit ($1, List.rev $2, $4) }

(*----------------------------*)
(* Method *)
(*----------------------------*)

method_declaration: method_header method_body  { { $1 with m_body = $2 } }

method_header:
 | modifiers_opt type_ method_declarator optl(throws)
     { method_header $1 $2 $3 $4 }
 | modifiers_opt VOID method_declarator optl(throws)
     { method_header $1 (void_type $2) $3 $4 }

method_declarator:
 | identifier "(" formal_parameter_list_opt ")"  { (IdentDecl $1), $3 }
 | method_declarator LB_RB                     { (ArrayDecl (fst $1)), snd $1 }

method_body:
 | block  { $1 }
 | ";"     { EmptyStmt $1 }


throws: THROWS qualified_ident_list (* was class_type_list *)  
  { List.map typ_of_qualified_id $2 }


generic_method_or_constructor_decl:
  modifiers_opt type_parameters generic_method_or_constructor_rest  { ast_todo }

generic_method_or_constructor_rest:
 | type_ identifier method_declarator_rest { }
 | VOID identifier method_declarator_rest { }

method_declarator_rest:
 | formal_parameters optl(throws) method_body { }

(*----------------------------*)
(* Constructors *)
(*----------------------------*)

constructor_declaration:
 modifiers_opt constructor_declarator optl(throws) constructor_body
  {
    let (id, formals) = $2 in
    let var = { mods = $1; type_ = None; name = id } in
    Method { m_var = var; m_formals = formals; m_throws = $3;
	     m_body = $4 }
  }

constructor_declarator:	identifier "(" formal_parameter_list_opt ")"  { $1, $3 }

constructor_body:
 | "{" block_statement* "}"                                 
    { Block ($1, List.flatten $2, $3) }
 | "{" explicit_constructor_invocation block_statement* "}" 
    { Block ($1, $2::(List.flatten $3), $4) }


explicit_constructor_invocation:
 | THIS "(" argument_list_opt ")" ";"
      { constructor_invocation [this_ident $1] ($2,$3,$4) $5 }
 | SUPER "(" argument_list_opt ")" ";"
      { constructor_invocation [super_ident $1] ($2,$3,$4) $5 }
 (* javaext: ? *)
 | primary "." SUPER "(" argument_list_opt ")" ";"
      { Expr (Call ((Dot ($1, $2, super_identifier $3)), ($4,$5,$6)), $7) }
 (* not in 2nd edition java language specification. *)
 | name "." SUPER "(" argument_list_opt ")" ";"
      { constructor_invocation (name $1 @ [super_ident $3]) ($4,$5,$6) $7 }

(*----------------------------*)
(* Method parameter *)
(*----------------------------*)

formal_parameters: "(" formal_parameter_list_opt ")" { $2 }

formal_parameter: 
 | variable_modifier* type_ variable_declarator_id_bis
  { ParamClassic (canon_var $1 (Some $2) $3) }
 (* sgrep-ext: *)
 | "..." { ParamEllipsis $1 }

variable_declarator_id_bis:
 | variable_declarator_id      { $1 }
 (* javaext: 1.? *)
 | "..." variable_declarator_id { $2 (* todo_ast *) }

 (* javaext: 1.? *)
variable_modifier:
 | FINAL      { Final, $1 }
 | annotation { (Annotation $1), Common2.fst3 $1 }

(*************************************************************************)
(* Interface *)
(*************************************************************************)

interface_declaration:
 modifiers_opt INTERFACE identifier type_parameters_opt  extends_interfaces_opt
 interface_body
  { { cl_name = $3; cl_kind = Interface;
      cl_mods = $1; cl_tparams = $4;
      cl_extends = None; cl_impls = $5;
      cl_body = $6;
    }
  }

extends_interfaces:
 | EXTENDS reference_type (* was interface_type *) { [$2] }
 | extends_interfaces "," reference_type  { $1 @ [$3] }

(*----------------------------*)
(* Interface body *)
(*----------------------------*)

interface_body: "{" interface_member_declaration* "}" 
  { $1, List.flatten $2, $3 }

interface_member_declaration:
 | constant_declaration  { $1 }
 (* javaext: was abstract_method_declaration *)
 | interface_method_declaration  { [Method $1] }

 (* javaext: 1.? *)
 | interface_generic_method_decl { $1 }

 (* javaext: 1.? *)
 | class_declaration      { [Class $1] }
 | interface_declaration  { [Class $1] }

 (* javaext: 1.? *)
 | enum_declaration       { [Enum $1] }

 (* javaext: 1.? *)
 | annotation_type_declaration { [Class $1] }

 | ";"  { [] }


(* note: semicolon is missing in 2nd edition java language specification.*)
(* less: could replace with field_declaration? was field_declaration *)
constant_declaration: modifiers_opt type_ variable_declarators ";"
     { decls (fun x -> Field x) $1 $2 (List.rev $3) }

(* javaext:: was abstract_method_declaration only before *)
interface_method_declaration: method_declaration { $1 }

interface_generic_method_decl:
 | modifiers_opt type_parameters type_ identifier interface_method_declator_rest
    { ast_todo }
 | modifiers_opt type_parameters VOID identifier interface_method_declator_rest
    { ast_todo }

interface_method_declator_rest:
 | formal_parameters optl(throws) ";" { }

(*************************************************************************)
(* Enum *)
(*************************************************************************)

enum_declaration: modifiers_opt ENUM identifier optl(interfaces) enum_body
   { { en_name = $3; en_mods = $1; en_impls = $4; en_body = $5; } }

(* cant factorize in enum_constants_opt comma_opt .... *)
enum_body:
 | "{"                   optl(enum_body_declarations) "}" { [], $2 }
 | "{" enum_constants    optl(enum_body_declarations) "}" { $2, $3 }
 | "{" enum_constants "," optl(enum_body_declarations) "}" { $2, $4 }

enum_constant: modifiers_opt enum_constant_bis { $2 }

enum_constant_bis:
 | identifier                         { $1, None, None }
 | identifier "(" argument_list_opt ")" { $1, Some ($2,$3,$4), None }
 | identifier "{" method_declaration* "}"  
    { $1, None, Some ($2, $3 |> List.map (fun x -> Method x) , $4) }

enum_body_declarations: ";" class_body_declaration* { List.flatten $2 }

(*************************************************************************)
(* Annotation type decl *)
(*************************************************************************)

annotation_type_declaration:
  modifiers_opt "@" INTERFACE identifier annotation_type_body 
     { { cl_name = $4; cl_kind = AtInterface; cl_mods = $1; cl_tparams = [];
         cl_extends = None; cl_impls = []; cl_body = $5 }
     }

annotation_type_body: "{" annotation_type_element_declarations_opt "}" 
  { $1, $2, $3 }

annotation_type_element_declaration:
 annotation_type_element_rest { $1 }

annotation_type_element_rest:
 | modifiers_opt type_ identifier annotation_method_or_constant_rest ";" 
   { AnnotationTypeElementTodo (snd $3) }

 | class_declaration           { Class $1 }
 | enum_declaration            { Enum $1 }
 | interface_declaration       { Class $1 }
 | annotation_type_declaration { Class $1 }


annotation_method_or_constant_rest:
 | "(" ")" { }
 | "(" ")" DEFAULT element_value { }

annotation_type_element_declarations_opt:
 | (*empty*) { [] }
 | annotation_type_element_declarations { $1 }

annotation_type_element_declarations:
 | annotation_type_element_declaration { [$1] }
 | annotation_type_element_declarations annotation_type_element_declaration 
    { $1 @ [$2] }

(*************************************************************************)
(* xxx_list, xxx_opt *)
(*************************************************************************)

(* can't use modifier*, need %inline and separate modifiers rule *)
%inline
modifiers_opt:
 | (*empty*)  { [] }
 | modifiers  { List.rev $1 }

modifiers: 
 | modifier  { [$1] }
 | modifiers modifier  { $2 :: $1 }

(* basic lists, at least one element *)

switch_block_statement_groups:
 | switch_block_statement_group  { [$1] }
 | switch_block_statement_groups switch_block_statement_group  { $2 :: $1 }

(* basic lists, at least one element with separator *)
ref_type_list:
 | reference_type  { [$1] }
 | ref_type_list "," reference_type  { $1 @ [$3] }

resource_list:
 | resource  { [$1] }
 | resource_list ";" resource  { $1 @ [$3] }

ref_type_and_list:
 | reference_type  { [$1] }
 | ref_type_and_list AND reference_type  { $1 @ [$3] }

variable_declarators:
 | variable_declarator  { [$1] }
 | variable_declarators "," variable_declarator  { $3 :: $1 }

formal_parameter_list:
 | formal_parameter  { [$1] }
 | formal_parameter_list "," formal_parameter  { $3 :: $1 }

variable_initializers:
 | variable_initializer  { [$1] }
 | variable_initializers "," variable_initializer  { $3 :: $1 }

qualified_ident_list:
 | name                          { [qualified_ident $1] }
 | qualified_ident_list "," name  { $1 @ [qualified_ident $3] }

statement_expression_list:
 | statement_expression                               { [$1] }
 | statement_expression_list "," statement_expression  { $1 @ [$3] }

argument_list:
 | argument  { [$1] }
 | argument_list "," argument  { $3 :: $1 }

enum_constants:
 | enum_constant { [$1] }
 | enum_constants "," enum_constant { $1 @ [$3] }

type_parameters_bis:
 | type_parameter                         { [$1] }
 | type_parameters_bis "," type_parameter  { $1 @ [$3] }

type_arguments_args:
 | type_argument                    { [$1] }
 | type_arguments_args "," type_argument  { $1 @ [$3] }

element_value_pairs:
 | element_value_pair { [$1] }
 | element_value_pairs "," element_value_pair { $1 @ [$3] }

element_values:
 | element_value { [$1] }
 | element_values "," element_value { $1 @ [$3] }


(* basic lists, 0 element allowed *)


formal_parameter_list_opt:
 | (*empty*)  { [] }
 | formal_parameter_list  { List.rev $1 }

extends_interfaces_opt:
 | (*empty*)  { [] }
 | extends_interfaces  { $1 }


argument_list_opt:
 | (*empty*)  { [] }
 | argument_list  { List.rev $1 }

dims_opt:
 | (*empty*)  { 0 }
 | dims  { $1 }

type_parameters_opt:
 | (*empty*)   { [] }
 | type_parameters { $1 }

type_arguments_args_opt:
 | (*empty*)   { [] }
 | type_arguments_args { $1 }

