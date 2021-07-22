(* Yoann Padioleau
 *
 * Copyright (C) 2012 Yoann Padioleau
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
*)

let logger = Logging.get_logger [__MODULE__]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Just a small wrapper around the C++ parser
 *)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let parse file =
  let (ast2, stat) = Parse_cpp.parse_with_lang ~lang:Flag_parsing_cpp.C file in
  let ast = ast2 |> Ls.map fst in
  let toks = ast2 |> List.concat_map snd in
  let ast, stat =
    try (Ast_c_build.program ast), stat
    with exn ->
      logger#error "PB: Ast_c_build, on %s (exn = %s)" file (Common.exn_to_s exn);
      (*None, { stat with Stat.bad = stat.Stat.bad + stat.Stat.correct } *)
      raise exn
  in
  { Parse_info. ast; tokens = toks; stat }

let parse_program file =
  let res = parse file in
  res.Parse_info.ast

let any_of_string str =
  let any = Parse_cpp.any_of_string Flag_parsing_cpp.C str in
  Ast_c_build.any any
