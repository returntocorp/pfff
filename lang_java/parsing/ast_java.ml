(* Joust: a Java lexer, parser, and pretty-printer written in OCaml.
 * Copyright (C) 2001  Eric C. Cooper <ecc@cmu.edu>
 * Released under the GNU General Public License
 *
 * Extended by Yoann Padioleau to support more recent versions of Java.
 * Copyright (C) 2011 Facebook
 * Copyright (C) 2020 r2c
 *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* An AST for Java.
 *
 * For Java we directly do an AST, as opposed to a CST (Concrete
 * Syntax Tree) as in lang_php/. This should be enough for higlight_java.ml
 * I think (we just need the full list of tokens + the AST with position
 * for the identifiers).
 *
 * todo:
 *  - support generic methods (there is support for generic classes though)
 *  - Look for featherweight Java
 *  - look for middleweight Java (mentioned in Coccinelle4J paper)
 *
 * history:
 * - 2010 port to the pfff infrastructure.
 * - 2012 heavily modified to support annotations, generics, enum, foreach, etc
 * - 2020 support lambdas
 *)

(*****************************************************************************)
(* Names *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Token/info *)
(* ------------------------------------------------------------------------- *)
type tok = Parse_info.t
  (* with tarzan *)
type 'a wrap  = 'a * tok
  (* with tarzan *)

type 'a list1 = 'a list (* really should be 'a * 'a list *)
  (* with tarzan *)

(* round(), square[], curly{}, angle<> brackets *)
type 'a bracket = tok * 'a * tok
 (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Ident, qualifier *)
(* ------------------------------------------------------------------------- *)
(* for class/interface/enum names, method/field names, type parameter, ... *)
type ident = string wrap
 (* with tarzan *)

(* for package, import, throw specification *)
type qualified_ident = ident list
 (* with tarzan *)

(*****************************************************************************)
(* Type *)
(*****************************************************************************)
type typ =
  (* 'void', 'int', and other primitive types; could be merged with TClass *)
  | TBasic of string wrap
  | TClass of class_type
  | TArray of typ

  (* class or interface or enum type actually *)
 and class_type =
   (ident * type_argument list) list1

  and type_argument =
    | TArgument of ref_type
    | TQuestion of (bool (* extends|super, true = super *) * ref_type) option

   (* A ref type should be a class type or an array of whatever, but not a
    * primitive type. We don't enforce this invariant in the AST to simplify
    * things.
    *)
    and ref_type = typ
 (* with tarzan *)

type type_parameter =
  | TParam of ident * ref_type list (* extends *)
 (* with tarzan *)

(* ------------------------------------------------------------------------- *)
(* Modifier *)
(* ------------------------------------------------------------------------- *)
type modifier =
  | Public | Protected | Private
  | Abstract | Final
  | Static
  | Transient | Volatile | Native | StrictFP
  | Synchronized

  (* only for parameters, '...' *)
  | Variadic

  | Annotation of annotation

 and modifiers = modifier wrap list

(* ------------------------------------------------------------------------- *)
(* Annotation *)
(* ------------------------------------------------------------------------- *)
 and annotation = name_or_class_type * (annotation_element option)

 and annotation_element =
   | AnnotArgValue of element_value
   | AnnotArgPairInit of annotation_pair list
   | EmptyAnnotArg
 and element_value =
   | AnnotExprInit of expr
   | AnnotNestedAnnot of annotation
   | AnnotArrayInit of element_value list

 and annotation_pair = (ident * element_value)

 and name_or_class_type = identifier_ list
 and identifier_ =
   | Id of ident
   | Id_then_TypeArgs of ident * type_argument list
   | TypeArgs_then_Id of type_argument list * identifier_

(*****************************************************************************)
(* Expression *)
(*****************************************************************************)

(* When do we need to have a name with actual type_argument?
 * For certain calls like List.<Int>of(), which are rare.
 * less: do a NameGeneric instead? the type_argument could then be
 *  only at the end?
 *)
and name = (type_argument list * ident) list1

(* Can have nested anon class (=~ closures) in expressions hence
 * the use of type ... and ... below
 *)
and expr =
  (* Name is used for local variable, 'this' and 'super' special names,
   * and statically computable entities such as Package1.subpackage.Class.
   * Field or method accesses should use Dot (see below). Unfortunately
   * the Java grammar is ambiguous and without contextual information,
   * there is no way to know whether x.y.z is an access to the field z
   * of field y of local variable x or the static field z of class y
   * in package x. See the note on Dot below.
   *)
  | Name of name

 (* This is used only in the context of annotations *)
  | NameOrClassType of name_or_class_type

  | Literal of literal

  (* Xxx.class *)
  | ClassLiteral of typ

  (* the 'decls option' is for anon classes *)
  | NewClass of typ * arguments * decls bracket option
  (* the int counts the number of [], new Foo[][] => 2 *)
  | NewArray of typ * arguments * int * init option
  (* see tests/java/parsing/NewQualified.java *)
  | NewQualifiedClass of expr * ident * arguments * decls bracket option

  | Call of expr * arguments

  (* How is parsed X.y ? Could be a Name [X;y] or Dot (Name [X], y)?
   * The static part should be a Name and the more dynamic part a Dot.
   * So variable.field and variable.method should be parsed as
   * Dot (Name [variable], field|method). Unfortunately
   * variable.field1.field2 is currently parsed as
   * Dot (Name [variable;field1], field2). You need semantic information
   * about variable to disambiguate.
   *
   * Why the ambiguity? Names and packages are not
   * first class citizens, so one cant pass a class/package name as an
   * argument to a function, so when have X.Y.z in an expression, the
   * last element has to be a field or a method (if it's a class,
   * people should use X.Y.class), so it's safe to transform such
   * a Name at parsing time in a Dot.
   * The problem is that more things in x.y.z can be a Dot, but to know
   * that requires semantic information about the type of x and y.
   *)
  | Dot of expr * tok * ident

  | ArrayAccess of expr * expr

  | Unary of Ast_generic.arithmetic_operator (* +/-/~/! *) wrap * expr
  | Postfix of expr * Ast_generic.incr_decr wrap
  | Prefix of Ast_generic.incr_decr wrap * expr
  | Infix of expr * Ast_generic.arithmetic_operator wrap * expr

  | Cast of typ * expr

  | InstanceOf of expr * ref_type

  | Conditional of expr * expr * expr
  (* ugly java, like in C assignement is an expression not a statement :( *)
  | Assign of expr * tok * expr
  | AssignOp of expr * Ast_generic.arithmetic_operator wrap * expr

  (* javaext: 1.? *)
  | Lambda of parameters * stmt

  (* sgrep-ext: *)
  | Ellipsis of tok

  and literal = 
  | Int of string wrap
  | Float of string wrap
  | String of string wrap
  | Char of string wrap
  | Bool of bool wrap
  | Null of tok

and arguments = expr list

(*****************************************************************************)
(* Statement *)
(*****************************************************************************)

and stmt =
  | Empty (* could be Block [] *)
  | Block of stmts
  | Expr of expr

  | If of tok * expr * stmt * stmt
  | Switch of tok * expr * (cases * stmts) list

  | While of tok * expr * stmt
  | Do of tok * stmt * expr
  | For of tok * for_control * stmt

  | Break of tok * ident option
  | Continue of tok * ident option
  | Return of tok * expr option
  | Label of ident * stmt

  | Sync of expr * stmt

  | Try of tok * stmt * catches * (tok * stmt) option
  | Throw of tok * expr

  (* decl as statement *)
  | LocalVar of var_with_init

  | LocalClass of class_decl

  (* javaext: http://java.sun.com/j2se/1.4.2/docs/guide/lang/assert.html *)
  | Assert of tok * expr * expr option (* assert e or assert e : e2 *)

and stmts = stmt list

and case =
  | Case of (tok * expr)
  | Default of tok
and cases = case list

and for_control =
  | ForClassic of for_init * expr list * expr list
  | Foreach of var_definition * expr
  and for_init =
    | ForInitVars of var_with_init list
    | ForInitExprs of expr list

and catch = tok * var_definition * stmt
and catches = catch list

(*****************************************************************************)
(* Definitions *)
(*****************************************************************************)
(* todo: use entity to factorize fields in method_decl, class_decl *)
and entity = {
    name: ident;
    mods: modifiers;
    type_: typ option;
    (* todo? tparams: type_parameter list; *)
}

(* ------------------------------------------------------------------------- *)
(* variable (local var, parameter) declaration *)
(* ------------------------------------------------------------------------- *)
and var_definition = entity

and vars = var_definition list

(* less: could be merged with var *)
and var_with_init = {
  f_var: var_definition;
  f_init: init option
}

  (* less: could merge with expr *)
  and init =
    | ExprInit of expr
    | ArrayInit of init list bracket

(* ------------------------------------------------------------------------- *)
(* Methods, fields *)
(* ------------------------------------------------------------------------- *)

(* method or constructor *)
and method_decl = {
  (* m_var.type_ is None for a constructor *)
  m_var: var_definition;
  (* the var.mod in params can only be Final or Annotation *)
  m_formals: parameters;
  m_throws: qualified_ident list;

  (* todo: m_tparams *)

  (* Empty for methods in interfaces.
   * For constructor the first stmts can contain
   * explicit_constructor_invocations.
   *)
  m_body: stmt
}

  and parameters = parameter_binding list
    and parameter_binding = 
     | ParamClassic of parameter
     (* sgrep-ext: *)
     | ParamEllipsis of tok
    and parameter = var_definition

and field = var_with_init

(* ------------------------------------------------------------------------- *)
(* Enum *)
(* ------------------------------------------------------------------------- *)

and enum_decl = {
  en_name: ident;
  en_mods: modifiers;
  en_impls: ref_type list;
  en_body: enum_constant list * decls;
}
 and enum_constant =
   | EnumSimple of ident
   (* http://docs.oracle.com/javase/1.5.0/docs/guide/language/enums.html *)
   | EnumConstructor of ident * arguments
   | EnumWithMethods of ident * method_decl list

(* ------------------------------------------------------------------------- *)
(* Class/Interface *)
(* ------------------------------------------------------------------------- *)

and class_decl = {
  cl_name: ident;
  cl_kind: class_kind;

  cl_tparams: type_parameter list;

  cl_mods: modifiers;

  (* always at None for interface *)
  cl_extends: typ option;
  (* for interface this is actually the extends *)
  cl_impls: ref_type list;

  (* javaext: the methods body used to be always empty for interface *)
  cl_body: decls bracket;
}
  and class_kind = ClassRegular | Interface
(*****************************************************************************)
(* Declaration *)
(*****************************************************************************)

and decl =
  (* top decl *)
  | Class of class_decl
  | Enum of enum_decl

  | Method of method_decl
  | Field of field
  | Init of bool (* static *) * stmt
  (* sgrep-ext: allows ... inside interface, class declerations *)
  | DeclEllipsis of tok

and decls = decl list

 (* with tarzan *)

(*****************************************************************************)
(* Toplevel *)
(*****************************************************************************)
type import = 
  | ImportAll of tok * qualified_ident * tok (* * *)
  | ImportFrom of tok * qualified_ident * ident

type compilation_unit = {
  package: (tok * qualified_ident) option;
  (* The qualified ident can also contain "*" at the very end.
   * The bool is for static import (javaext:)
   *)
  imports: (bool * import) list;
  (* todo? necessarily a (unique) class/interface first? *)
  decls: decls;
}
 (* with tarzan *)

type program = compilation_unit
 (* with tarzan *)

(*****************************************************************************)
(* Any *)
(*****************************************************************************)

type any =
  | AIdent of ident
  | AExpr of expr
  | AStmt of stmt
  | AStmts of stmt list
  | ATyp of typ
  | AVar of var_definition
  | AInit of init
  | AMethod of method_decl
  | AField of field
  | AClass of class_decl
  | ADecl of decl
  | ADecls of decls
  | AProgram of program
 (* with tarzan *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let unwrap = fst

let fakeInfo ?(next_to=None) str = { Parse_info.
  token = Parse_info.FakeTokStr (str, next_to);
  transfo = Parse_info.NoTransfo;
}

let ast_todo = []
let ast_todo2 = ()

let info_of_ident ident =
  snd ident

let is_final xs =
  let xs = List.map fst xs in
  List.mem Final xs
let is_final_static xs =
  let xs = List.map fst xs in
  List.mem Final xs && List.mem Static xs

let rec info_of_identifier_ (id : identifier_) : tok = match id with
  | Id id
  | Id_then_TypeArgs (id, _) -> snd id
  | TypeArgs_then_Id (_, id_) -> info_of_identifier_ id_
