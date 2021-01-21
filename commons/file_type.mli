
type file_type =
  | PL of pl_type
  | Obj of string
  | Binary of string
  | Text of string
  | Doc of string
  | Config of config_type
  | Media of media_type
  | Archive of string
  | Other of string

and pl_type =
  | ML of string | Haskell of string | Lisp of lisp_type | Skip
  | Prolog of string
  | Script of string
  | C of string | Cplusplus of string | Java | Kotlin | Csharp | ObjectiveC of string
  | Swift
  | Perl | Python | Ruby | Lua | R
  | Erlang | Go | Rust
  | Beta
  | Pascal
  | Web of webpl_type
  | Haxe | Opa | Flash
  | Bytecode of string
  | Asm
  | Thrift
  | MiscPL of string

and config_type =
  | Makefile
  | Json
  | Jsonnet
  | Yaml

and lisp_type = CommonLisp | Elisp | Scheme | Clojure

and webpl_type =
  | Php of string
  | Js | Coffee | TypeScript | TSX
  | Css
  | Html | Xml
  | Sql

and media_type =
  | Sound of string
  | Picture of string
  | Video of string


val file_type_of_file:
  Common.filename -> file_type

val is_textual_file:
  Common.filename -> bool
val is_syncweb_obj_file:
  Common.filename -> bool
val is_json_filename:
  Common.filename -> bool

val files_of_dirs_or_files:
  (file_type -> bool) -> Common.filename list -> Common.filename list

(* specialisations *)
val webpl_type_of_file:
  Common.filename -> webpl_type option

(* val string_of_pl: pl_kind -> string *)
