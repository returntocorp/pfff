
val vof_info: Parse_info.t -> Ocaml.v

type dumper_precision = {
  full_info: bool;
  token_info: bool;
  type_info: bool;
}
val default_dumper_precision : dumper_precision

val _current_precision: dumper_precision ref
val vof_info_adjustable_precision: Parse_info.t -> Ocaml.v

val cmdline_flags_precision: unit -> Common.flag_spec list
