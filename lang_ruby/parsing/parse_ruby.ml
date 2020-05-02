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
open Common
module Flag = Flag_parsing
module PI = Parse_info
module TH = Token_helpers_ruby

module H = Ast_ruby_helpers
module HH = Parser_ruby_helpers
module Utils = Utils_ruby

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(* the token list contains also the comment-tokens *)
type program_and_tokens = 
  Ast_ruby.program option * Parser_ruby.token list (* may be partial *)

(*****************************************************************************)
(* Error diagnostic  *)
(*****************************************************************************)
let error_msg_tok tok = 
  Parse_info.error_message_info (TH.info_of_tok tok)

(*****************************************************************************)
(* Lexing only *)
(*****************************************************************************)

(* todo? reuse Parse_info.tokenize_all_and_adjust_pos, but that
 * would require to have completely independent lexer and parser
 * which seems not possible with Ruby.
 *)
let mk_lexer file chan =

  let lexbuf = Lexing.from_channel chan in
  let state = Lexer_parser_ruby.create Lexer_ruby.top_lexer in 

  let table     = Parse_info.full_charpos_to_pos_large file in

  let adjust_info ii = 
    { ii with PI.token =
      (* could assert pinfo.filename = file ? *)
       match ii.PI.token with
       | PI.OriginTok pi -> 
          (try 
            PI.OriginTok
              (PI.complete_token_location_large file table pi)
           with Invalid_argument("index out of bounds") ->
              (* TODO: fix! *)
              (* pr2_gen pi *)
              pr2_once (spf "TODO:%s: adjust info out-of-bounds" file);
              PI.OriginTok pi
           )
       | _ -> failwith "adjust_info: no an OriginTok"
    }
  in
  let toks = ref [] in

  (* set the environment *)
  HH.clear_env ();
  let env = Utils.default_opt Utils.StrSet.empty None in
  HH.set_env env;
  
  let rec lexer lexbuf = 
    let tok = 
     try
       Lexer_ruby.token state lexbuf
     with PI.Lexical_error (s, info) ->
       raise (PI.Lexical_error (s, adjust_info info))
    in 
    if !Flag_parsing.debug_lexer
    then Common.pr2_gen tok;

    let tok = tok |> TH.visitor_info_of_tok adjust_info in
    Common.push tok toks;
    if TH.is_comment tok
    then lexer lexbuf
    else tok
  in
  toks, lexbuf, lexer

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let parse file = 
  let stat = Parse_info.default_stat file in
  let n = Common2.nblines_file file in

  Common.with_open_infile file (fun chan -> 
    let toks, lexbuf, lexer = mk_lexer file chan in
    try 
      (* -------------------------------------------------- *)
      (* Call parser *)
      (* -------------------------------------------------- *)
      let lst = 
          (* GLR parsing can be very time consuming *)
          Common.timeout_function 5 (fun () ->
            Parser_ruby.main lexer lexbuf 
          )
      in

      (* check for ambiguous parse trees *)
      let l = List.map fst lst in
      let l' = HH.uniq_list (fun a b -> if H.equal_ast a b then 0 else -1) l in
      HH.do_fail "program" l'
          Ast_ruby_printer.string_of_ast Meta_ast_ruby.vof_program;

      let ast = List.hd l' in
      (*orig-todo? Ast.mod_ast (replace_heredoc state) ast*) 
      stat.PI.correct <- n;
      (Some ast, List.rev !toks), stat

    with (Dyp.Syntax_error 
         | Failure _ | Stack.Empty | Common.Timeout
         ) as exn ->
      let cur = 
        match !toks with
        | [] -> failwith (spf "No token at all for %s" file)
        | x::_xs -> x
      in
      (* todo: need to fix those! *)
      let s = Common.exn_to_s exn in
      pr2 (spf "Exn on %s = %s" file s);

      if not !Flag.error_recovery && exn = Dyp.Syntax_error
      then raise (PI.Parsing_error (TH.info_of_tok cur));
      if not !Flag.error_recovery && exn <> Dyp.Syntax_error
      then raise (PI.Other_error (s, TH.info_of_tok cur));
  
      if !Flag.show_parsing_error && exn = Dyp.Syntax_error
      then begin
        pr2 ("parse error \n = " ^ error_msg_tok cur);
        let filelines = Common2.cat_array file in
        let checkpoint2 = Common.cat file |> List.length in
        let line_error = PI.line_of_info (TH.info_of_tok cur) in
        Parse_info.print_bad line_error (0, checkpoint2) filelines;
      end;
  
      stat.PI.bad <- n;
      if exn = Common.Timeout then stat.PI.have_timeout <- true;
      (None, List.rev !toks), stat
  )

let parse_program file =
  let ((ast, _toks), _stat) = parse file in
  Common2.some ast
