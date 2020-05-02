(*s: pfff/lang_GENERIC/parsing/visitor_ast.mli *)
open Ast_generic

(* the hooks *)
(*s: type [[Visitor_ast.visitor_in]] *)
type visitor_in = {
  kexpr: (expr  -> unit) * visitor_out -> expr  -> unit;
  kstmt: (stmt  -> unit) * visitor_out -> stmt  -> unit;
  ktype_: (type_  -> unit) * visitor_out -> type_  -> unit;
  kpattern: (pattern  -> unit) * visitor_out -> pattern  -> unit;

  kdef: (definition  -> unit) * visitor_out -> definition  -> unit;
  kdir: (directive  -> unit) * visitor_out -> directive  -> unit;

  kattr: (attribute  -> unit) * visitor_out -> attribute  -> unit;
  kparam: (parameter  -> unit) * visitor_out -> parameter  -> unit;
  kident: (ident -> unit)  * visitor_out -> ident  -> unit;
  kentity: (entity -> unit)  * visitor_out -> entity  -> unit;
  kstmts: (stmt list  -> unit) * visitor_out -> stmt list -> unit;

  kfunction_definition: (function_definition -> unit) * visitor_out -> 
    function_definition -> unit;
  kclass_definition: (class_definition -> unit) * visitor_out -> 
    class_definition -> unit;

  kinfo: (tok -> unit)  * visitor_out -> tok  -> unit;
}
(*e: type [[Visitor_ast.visitor_in]] *)
(*s: type [[Visitor_ast.visitor_out]] *)
and visitor_out = any -> unit
(*e: type [[Visitor_ast.visitor_out]] *)

(*s: signature [[Visitor_ast.default_visitor]] *)
val default_visitor : visitor_in
(*e: signature [[Visitor_ast.default_visitor]] *)

(*s: signature [[Visitor_ast.mk_visitor]] *)
val mk_visitor: visitor_in -> visitor_out
(*e: signature [[Visitor_ast.mk_visitor]] *)

(* poor's man fold *)
(* 
val do_visit_with_ref:
  ('a list ref -> visitor_in) -> any -> 'a list
*)
(*e: pfff/lang_GENERIC/parsing/visitor_ast.mli *)
