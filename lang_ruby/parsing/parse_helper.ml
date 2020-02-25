module Ast = Ast_ruby
module H = Ast_ruby_helpers

let set_lexbuf_fname lexbuf name = 
  lexbuf.Lexing.lex_curr_p <-
    {lexbuf.Lexing.lex_curr_p with
       Lexing.pos_fname = name
    }

let set_lexbuf_lineno lexbuf line = 
  lexbuf.Lexing.lex_curr_p <-
    {lexbuf.Lexing.lex_curr_p with
       Lexing.pos_lnum = line
    }

let uniq_list lst =
  let rec u = function
    | [] -> []
    | [x] -> [x]
    | x1::x2::tl ->
	if H.equal_ast x1 x2
	then u (x1::tl) else x1 :: (u (x2::tl))
  in
  let l = List.map fst lst in
    u (List.sort H.compare_ast l)

let rec parse_lexbuf_with_state ?env state lexbuf = 
  try 
    NewParser.clear_env ();
    let env = Utils.default_opt Utils.StrSet.empty env in
    let () = NewParser.set_env env in
    let lst = NewParser.main (NewLexer.token state) lexbuf in
    let lst = uniq_list lst in
      begin match lst with
        | [ast] -> (*Ast.mod_ast (replace_heredoc state) ast*) 
            ast
        | _l -> failwith "ambiguous parse"
      end
  with Dyp.Syntax_error ->
    let msg = Printf.sprintf "parse error in file %s, line: %d, token: '%s'\n"
      lexbuf.Lexing.lex_curr_p.Lexing.pos_fname
      lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum
      (Lexing.lexeme lexbuf)
    in
      failwith msg

and parse_string_with_state state ?env ?filename ?lineno str = 
  let lexbuf = Lexing.from_string str in
  let fname = match filename with None -> str | Some x -> x in
  let line = match lineno with None -> 1 | Some x -> x in 
    set_lexbuf_fname lexbuf fname;
    set_lexbuf_lineno lexbuf line;
    parse_lexbuf_with_state ?env state lexbuf

let parse_lexbuf lexbuf = 
  let state = RubyLexerState.create NewLexer.top_lexer in 
    parse_lexbuf_with_state state lexbuf

let parse_string ?env ?filename ?lineno str = 
  let state = RubyLexerState.create NewLexer.top_lexer in 
    parse_string_with_state state ?env ?filename ?lineno str

let parse_file fname = 
  let ic = open_in fname in
  let lexbuf = Lexing.from_channel ic in
  let () = set_lexbuf_fname lexbuf fname in
    try
      let res = parse_lexbuf lexbuf in
        close_in ic;
        res
    with e -> close_in ic; raise e
