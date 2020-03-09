{
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
open Parser_ruby (* the tokens *)
module S2 = Parser_ruby_helpers
module S = Lexer_parser_ruby
module Utils = Utils_ruby

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The Ruby lexer.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* shortcuts *)
let _tok = Lexing.lexeme
let tk lexbuf = Parse_info.tokinfo lexbuf
let _error = Parse_info.lexical_error

(* ---------------------------------------------------------------------- *)
(* Lexer/Parser state *)
(* ---------------------------------------------------------------------- *)
(* See lexer_parser_ruby.ml *)

let pop_lexer state = 
  Stack.pop state.S.lexer_stack |> ignore

(* emit the token [tok] and then proceed with the continuation [k] *)
let emit_extra tok k state lexbuf = 
  let once state _ = 
    pop_lexer state;
    k state lexbuf
  in
  Stack.push once state.S.lexer_stack;
  tok

(* helper for transitioning between states *)
let beg_choose want yes no state lexbuf = 
  if state.S.expr_state == want 
  then yes state lexbuf else no state lexbuf
    
type chooser = S.cps_lexer -> S.cps_lexer -> S.cps_lexer

(* state transition functions *)
let on_beg : chooser = beg_choose S.Expr_Beg
let on_mid : chooser = beg_choose S.Expr_Mid 
let on_end : chooser = beg_choose S.Expr_End 
let on_def : chooser = beg_choose S.Expr_Def 
let on_local : chooser = beg_choose S.Expr_Local 

(* CPS terminators *)
let t_uminus s lb     = S.beg_state s; T_UMINUS (tk lb)
let t_minus  s lb     = S.beg_state s; T_MINUS (tk lb)
let t_uplus  s lb     = S.beg_state s; T_UPLUS (tk lb)
let t_plus   s lb     = S.beg_state s; T_PLUS (tk lb)
let t_ustar  s lb     = S.beg_state s; T_USTAR (tk lb)
let t_star   s lb     = S.beg_state s; T_STAR (tk lb)
let t_uamper s lb     = S.beg_state s; T_UAMPER (tk lb)
let t_amper  s lb     = S.beg_state s; T_AMPER (tk lb)
let t_slash  s lb     = S.beg_state s; T_SLASH (tk lb)
let t_quest  s lb     = S.beg_state s; T_QUESTION (tk lb)
let t_tilde  s lb     = S.beg_state s; T_TILDE (tk lb)
let t_scope  s lb     = S.beg_state s; T_SCOPE (tk lb)
let t_uscope s lb     = S.beg_state s; T_USCOPE (tk lb)
let t_lbrack s lb     = S.beg_state s; T_LBRACK (tk lb)
let t_lbrack_arg s lb = S.beg_state s; T_LBRACK_ARG (tk lb)
let t_lparen s lb     = S.beg_state s; T_LPAREN (tk lb)
let t_lparen_arg s lb = S.beg_state s; T_LPAREN_ARG (tk lb)
let t_lbrace s lb     = S.beg_state s; T_LBRACE (tk lb)
let t_lbrace_arg s lb = S.beg_state s; T_LBRACE_ARG (tk lb)
let t_percent s lb    = S.beg_state s; T_PERCENT (tk lb)
let t_lshft s lb      = S.beg_state s; T_LSHFT (tk lb)
let t_colon s lb      = S.beg_state s; T_COLON (tk lb)
let t_eol s lb       = S.beg_state s; T_EOL (tk lb)

(* transitions to End unless in Def, in which case do nothing (stays in Def) *)
let def_end_state state = match state.S.expr_state with
  | S.Expr_Def -> ()
  | _ -> S.end_state state

(* ---------------------------------------------------------------------- *)
(* Lexing state *)
(* ---------------------------------------------------------------------- *)

let update_pos str pos = 
  let chars = String.length str in
  {pos with Lexing.pos_cnum = pos.Lexing.pos_cnum - chars; }

(* for heredoc *)
let push_back str lexbuf = 
  (* todo: just use Parse_info.yyback? need lex_buffer modifications? *)
  let pre_str = Bytes.sub lexbuf.Lexing.lex_buffer 0 lexbuf.Lexing.lex_curr_pos in
  let post_str = 
    Bytes.sub lexbuf.Lexing.lex_buffer lexbuf.Lexing.lex_curr_pos
      (lexbuf.Lexing.lex_buffer_len-lexbuf.Lexing.lex_curr_pos)
  in
    lexbuf.Lexing.lex_buffer <- 
      Bytes.of_string (Bytes.to_string pre_str ^ str ^ Bytes.to_string post_str);
    lexbuf.Lexing.lex_buffer_len <- lexbuf.Lexing.lex_buffer_len + (String.length str);
    lexbuf.Lexing.lex_curr_p <- update_pos str lexbuf.Lexing.lex_curr_p;
    assert ((Bytes.length lexbuf.Lexing.lex_buffer) == lexbuf.Lexing.lex_buffer_len)

(* ---------------------------------------------------------------------- *)
(* Misc *)
(* ---------------------------------------------------------------------- *)

let fail_eof _f _state _lexbuf = 
  failwith "parse error: premature end of file"

    
let contents_of_str s = [Ast_ruby.StrChars s]

(* returns true if string following the modifier m (e.g., %m{...})
   should be parsed as a single quoted or double quoted (interpreted)
   string *)
let modifier_is_single = function
  | "r" -> false
  | "w" -> true
  | "W" -> false
  | "q" -> true
  | "Q" -> false
  | "x" -> false
  | "" -> false
  | m ->  failwith (Printf.sprintf "unhandled string modifier: %s" m)

(* Ruby numerics can include _ to separate arbitrary digits.  This
   returns a new string representing a numeric with all _s removed *)
let remove_underscores str = 
  let len = String.length str in
  let buf = Buffer.create len in
    String.iter
      (function
         | '.'
         | '-' (* needed for 2.3e-4 *)
         | '+' (* needed for 2.3e+4 *)
         | 'e'
         | ('a'..'f') | ('A'..'F') | 'x'
         | ('0'..'9') as i -> Buffer.add_char buf i
         | '_' -> ()
         | _ -> failwith "non number and non-'_' in fixnum"
      ) str;
    Buffer.contents buf

let to_bignum str = Big_int.big_int_of_string (remove_underscores str)


(* base 16 -> base 10 helper *)
let num_of_hex_digit = function
  | '0' -> 0
  | '1' -> 1
  | '2' -> 2
  | '3' -> 3
  | '4' -> 4
  | '5' -> 5
  | '6' -> 6
  | '7' -> 7
  | '8' -> 8
  | '9' -> 9
  | 'a' | 'A' -> 10
  | 'b' | 'B' -> 11
  | 'c' | 'C' -> 12
  | 'd' | 'D' -> 13
  | 'e' | 'E' -> 14
  | 'f' | 'F' -> 15
  | c -> failwith (Printf.sprintf "num_of_hex_digit: %c" c)

(* converts the number stored in [str] initially represented in base
   [base] into a base 10 token (fixnum or bignum) *)
let convert_to_base10 ~base str pos = 
  let str = remove_underscores str in
  let base10 x exp = (* compute x * (base ** exp) *)
    Big_int.mult_big_int x 
      (Big_int.power_int_positive_int base exp)
  in
  let rec helper acc idx exp = 
    if idx < 0 then acc
    else
      let digit = num_of_hex_digit str.[idx] in
      let b10 = base10 (Big_int.big_int_of_int digit) exp in
      let acc' = Big_int.add_big_int b10 acc in
        helper acc' (idx - 1) (exp + 1)
  in
  let len = String.length str in
  let num = helper Big_int.zero_big_int (len-1) 0 in
    if Big_int.is_int_big_int num
    then T_FIXNUM(Big_int.int_of_big_int num, pos)
    else T_BIGNUM(num, pos)

let negate_numeric = function
  | T_FIXNUM(n,p) -> T_FIXNUM(-n,p)
  | T_FLOAT(s,n,p) -> T_FLOAT("-"^s,-.n,p)
  | T_BIGNUM(n,p) -> T_BIGNUM(Big_int.minus_big_int n,p)
  | _ -> assert false

let close_delim = function
  | '{' -> '}'
  | '<' -> '>'
  | '(' -> ')'
  | '[' -> ']'
  | _ -> assert false

(* chooses the T_LID or T_UID token based on the first character of [id] *)
let choose_capital_for_id id pos = 
  assert (String.length id > 0);
  match (id).[0] with
    | 'a'..'z' | '_' -> T_LID(id, pos)
    | 'A'..'Z' -> T_UID(id, pos)
    | _ -> failwith "unknown prefix char for ID: this shouldn't happen"

(* return a fresh Buffer.t that is preloaded with the contents of [str] *)
let buf_of_string str = 
  let b = Buffer.create 31 in
    Buffer.add_string b str;
    b
}

(*****************************************************************************)
(* Regexp aliases *)
(*****************************************************************************)

let ws = ['\t' ' '] 
let nl = '\n' | "\r\n"
let alpha = ['a'-'z''A'-'Z']
let num = ['0'-'9']

let alphanum = alpha | num
let rubynum = num ("_" | num)* 
let fixnum = rubynum
let post_fixnum = ("_" | num)* 

let rubyfloat = rubynum ('.' (rubynum))? ("e" ("-"|"+")? rubynum+)?
let post_rubyfloat = post_fixnum ('.' (rubynum))? ("e" ("-"|"+")? rubynum+)?

let id_start = '_' | alpha
let id_body = '_' | alphanum
let id_suffix = '?'| '!' (*('!' [^'=']?)*)
let id = id_start id_body* id_suffix?

let string_single_delim = [
  '!' '@' '#' '$' '%' '^' '&' '*'
  ',' '.' '?' '`' '~' '|' '+' '_'
  '-' '\\' '/' ':' '"' '\'']

let e = "" (* epsilon *)

(*****************************************************************************)
(* Rule initial *)
(*****************************************************************************)

rule token state = parse
  | e { if S2.begin_override() then S.beg_state state;
        (Stack.top state.S.lexer_stack) state lexbuf }

(*****************************************************************************)
(* Top_lexer *)
(*****************************************************************************)

and top_lexer state = parse

  (* ----------------------------------------------------------------------- *)
  (* spacing/comments *)
  (* ----------------------------------------------------------------------- *)
  | '#'   {comment state lexbuf}

  | "=begin" [^'\n']* '\n' {S.beg_state state; delim_comment state lexbuf}
  | "\\\n" { top_lexer state lexbuf}
  | nl     { t_eol state lexbuf}

  | ws+  { top_lexer state lexbuf}

  | "__END__"  { end_lexbuf lexbuf}

  (* ----------------------------------------------------------------------- *)
  (* symbols *)
  (* ----------------------------------------------------------------------- *)

  (* need the ws here to force longest match preference over the rules below *)
  | ws* ((['+' '-' '*' '&' '|' '%' '^'] | "||" | "&&" | "<<" | ">>") as op) '='
      {S.beg_state state; T_OP_ASGN(op, (tk lexbuf))}

  (* /= can be either regexp or op_asgn *)
  | ws* "/=" 
      { match state.S.expr_state with 
        | S.Expr_Beg -> regexp_string (buf_of_string "=") state lexbuf
        | _ -> S.beg_state state; T_OP_ASGN("/", (tk lexbuf))
      }

  (* need precedence over single form *)
  | ws* "**" {S.beg_state state;T_POW (tk lexbuf) }
  | ws* "&&" {S.beg_state state;T_ANDOP (tk lexbuf) }

  (* the following lexemes may represent various tokens depending on
     the expression state and surrounding spaces.  Space before and
     after is typically the binop form, while a space before but not
     after is uop.  *)
  | ws+ '-' 
      { let binop = on_def (postfix_at t_uminus t_minus) t_minus
        in space_uop uop_minus_lit t_uminus binop state lexbuf }
  | ws+ '+' 
      {let binop = on_def (postfix_at t_uplus t_plus) t_plus
       in space_uop uop_plus_lit t_uplus binop state lexbuf}
  | ws+ '*' {space_uop t_ustar t_ustar t_star state lexbuf}
  | ws+ '&' {space_uop t_uamper t_uamper t_amper state lexbuf}

  | ws+ '[' 
      { let binop = on_local t_lbrack_arg t_lbrack in 
        space_uop t_lbrack t_lbrack binop state lexbuf }
  | ws+ '(' 
      { let binop = on_local t_lparen_arg t_lparen in 
        space_uop t_lparen t_lparen binop state lexbuf}

  | ws+ '%' {space_tok percent t_percent state lexbuf}
  | ws+ '?' {space_tok char_code t_quest state lexbuf}
      
  (* no space is usually a binop, but is parsed as a uop at expr_beg
     or if there is a trailing @ in the def state *)
  | '-' {on_def (postfix_at t_uminus t_minus)
           (on_beg uop_minus_lit t_minus) state lexbuf}
  | '+' {on_def (postfix_at t_uplus t_plus)
           (on_beg uop_plus_lit t_plus) state lexbuf}
  | '*' {on_beg t_ustar  t_star state lexbuf }
  | '&' {on_beg t_uamper t_amper state lexbuf }
  
  | '(' {on_beg t_lparen t_lparen_arg state lexbuf}
  | '[' {on_beg t_lbrack t_lbrack_arg state lexbuf}
  | '{' {on_beg t_lbrace t_lbrace_arg state lexbuf}

  | ')'   {S.end_state state;T_RPAREN (tk lexbuf)}
  | ']'   {S.end_state state;T_RBRACK (tk lexbuf)}
  | '}'   {S.end_state state;T_RBRACE (tk lexbuf)}

  | '%' {on_beg percent t_percent state lexbuf}
  | '?' {on_beg char_code t_quest state lexbuf}

  (* need to explicitly separate out cases for / since spaces can
     be significant if they occur inside of a regexp *)
  | ws+ '/' (ws+ as spc)
      { on_beg (regexp_string (buf_of_string spc)) t_slash state lexbuf}
  | ws+ '/' (nl as nl)
      { on_beg (regexp_string (buf_of_string nl)) t_slash state lexbuf }
  | ws+ '/' { space_tok regexp t_slash state lexbuf}
  | '/'     { on_beg regexp t_slash state lexbuf}

  (* heredoc vs shift tokens *)
  | "<<-" {heredoc_header heredoc_string_lead state lexbuf}
  | "<<" ws {S.beg_state state; t_lshft state lexbuf}
  | "<<" nl {S.beg_state state; t_lshft state lexbuf}
  | "<<"    { match state.S.expr_state with
              | S.Expr_End | S.Expr_Local | S.Expr_Def -> 
                S.beg_state state;T_LSHFT (tk lexbuf)
              | S.Expr_Mid | S.Expr_Beg -> 
                heredoc_header heredoc_string state lexbuf}

  (* now all of the 'normal' tokens which are otherwise well behaved *)
  | '.'   {S.def_state state;T_DOT   (tk lexbuf) }
  | ','   {S.beg_state state;T_COMMA (tk lexbuf) }
  | ';'   {S.beg_state state;T_SEMICOLON (tk lexbuf)}

  | '!'   {S.beg_state state;T_BANG  (tk lexbuf) }
  | '~'   {match state.S.expr_state with
             (* when defining the ~ method, allow an optional @ postfix *)
             | S.Expr_Def -> postfix_at t_tilde t_tilde state lexbuf
             | _ -> t_tilde state lexbuf }

  | "<=>" {S.beg_state state;T_CMP (tk lexbuf) }
  | "="   {S.beg_state state;T_ASSIGN (tk lexbuf) }
  | "=="  {S.beg_state state;T_EQ (tk lexbuf) }
  | "===" {S.beg_state state;T_EQQ (tk lexbuf) }
  | "!="  {S.beg_state state;T_NEQ (tk lexbuf) }
  | ">="  {S.beg_state state;T_GEQ (tk lexbuf) }
  | "<="  {S.beg_state state;T_LEQ (tk lexbuf) }
  | "<"   {S.beg_state state;T_LT (tk lexbuf) }
  | ">"   {S.beg_state state;T_GT (tk lexbuf) }
  | "||"  {S.beg_state state;T_OROP (tk lexbuf) }
  | "=~"  {S.beg_state state;T_MATCH (tk lexbuf) }
  | "!~"  {S.beg_state state;T_NMATCH (tk lexbuf) }
  | ">>"  {S.beg_state state;T_RSHFT (tk lexbuf)}
  | "=>"  {S.beg_state state;T_ASSOC (tk lexbuf)}
  | '^'   {S.beg_state state;T_CARROT (tk lexbuf)}
  | '|'   {S.beg_state state;T_VBAR (tk lexbuf)}

  | "..." {S.beg_state state;T_DOT3 (tk lexbuf)}
  | ".."  {S.beg_state state;T_DOT2 (tk lexbuf)}

  | ":" {on_end t_colon atom state lexbuf}

  | ws+ "::" { T_USCOPE (tk lexbuf) }
  |     "::" { on_beg t_uscope t_scope state lexbuf }

  | ("@@" id) as id {S.end_state state; T_CLASS_VAR(id, (tk lexbuf))}
  | ('@' id)  as id {S.end_state state; T_INST_VAR(id, (tk lexbuf))}

  | '$'  { dollar state lexbuf}

  (* ----------------------------------------------------------------------- *)
  (* Constant *)
  (* ----------------------------------------------------------------------- *)

  | num { postfix_numeric Utils.id (Lexing.lexeme lexbuf) state lexbuf }

  (* ----------------------------------------------------------------------- *)
  (* Strings *)
  (* ----------------------------------------------------------------------- *)

  | '`'   {tick_string state lexbuf}
  | '\''  {S.end_state state; 
           non_interp_string '\'' (Buffer.create 31) state lexbuf}
  | '\"'  {double_string state lexbuf}

  (* ----------------------------------------------------------------------- *)
  (* Keywords *)
  (* ----------------------------------------------------------------------- *)

  | "class"    {S.def_state state;K_CLASS  (tk lexbuf)}
  | "def"      {S.def_state state;K_DEF    (tk lexbuf)}
  | "module"   {S.def_state state;K_MODULE (tk lexbuf)}
  | "alias"    {S.def_state state;K_ALIAS (tk lexbuf)}
  | "undef"    {S.def_state state;K_UNDEF (tk lexbuf)}
  | "and"      {S.beg_state state;K_AND (tk lexbuf)}
  | "begin"    {S.beg_state state;K_lBEGIN (tk lexbuf)}
  | "BEGIN"    {S.beg_state state;K_BEGIN (tk lexbuf)}
  | "case"     {S.beg_state state;K_CASE (tk lexbuf)}
  | "do"       {S.beg_state state;K_DO (tk lexbuf)}
  | "else"     {S.beg_state state;K_ELSE (tk lexbuf)}
  | "elsif"    {S.beg_state state;K_ELSIF (tk lexbuf)}
  | "END"      {S.beg_state state;K_END (tk lexbuf)}
  | "end"      {S.end_state state;K_lEND (tk lexbuf)}
  | "ensure"   {S.beg_state state;K_ENSURE (tk lexbuf)}
  | "for"      {S.beg_state state;K_FOR (tk lexbuf)}
  | "if"       {S.beg_state state;K_IF (tk lexbuf)}
  | "in"       {S.beg_state state;K_IN (tk lexbuf)}
  | "not"      {S.beg_state state;K_NOT (tk lexbuf)}
  | "or"       {S.beg_state state;K_OR (tk lexbuf)}
  | "rescue"   {S.beg_state state;K_RESCUE (tk lexbuf)}
  | "return"   {S.beg_state state;K_RETURN (tk lexbuf)}
  | "then"     {S.beg_state state;K_THEN (tk lexbuf)}
  | "unless"   {S.beg_state state;K_UNLESS (tk lexbuf)}
  | "until"    {S.beg_state state;K_UNTIL (tk lexbuf)}
  | "when"     {S.beg_state state;K_WHEN (tk lexbuf)}
  | "while"    {S.beg_state state;K_WHILE (tk lexbuf)}
  | "yield"    {S.mid_state state;K_YIELD (tk lexbuf)}
  | "nil"      {S.end_state state;K_NIL (tk lexbuf)}
  | "self"     {S.end_state state;K_SELF (tk lexbuf)}
  | "true"     {S.end_state state;K_TRUE (tk lexbuf)}
  | "false"    {S.end_state state;K_FALSE (tk lexbuf)}
(* No longer lex separately
  | "defined?" {S.mid_state state;K_DEFINED}
  | "super"    {S.mid_state state;K_SUPER}
  | "break"    {S.beg_state state;K_BREAK}
  | "redo"     {S.beg_state state;K_REDO}
  | "retry"    {S.beg_state state;K_RETRY}
  | "next"     {S.beg_state state;K_NEXT}
*)

  (* ----------------------------------------------------------------------- *)
  (* Ident *)
  (* ----------------------------------------------------------------------- *)
  | id as id { 
      let tok = choose_capital_for_id id (tk lexbuf) in
        begin match state.S.expr_state, tok with
          | S.Expr_Def, _ -> S.mid_state state
          | _, T_LID(id, _pos) ->
              if S2.assigned_id id 
              then S.local_state state
              else S.mid_state state
          | _ -> S.mid_state state
        end;
        tok
    }

  (* ----------------------------------------------------------------------- *)
  (* eof *)
  (* ----------------------------------------------------------------------- *)
  | eof   { T_EOF (tk lexbuf) }


(*****************************************************************************)
(* Comments *)
(*****************************************************************************)

and comment state = parse
  | [^'\n']* '\n' { t_eol state lexbuf }
  | [^'\n']* { comment state lexbuf }
  | eof { T_EOF (tk lexbuf) }

and delim_comment state = parse
  | "=end" [^'\n']* '\n' { top_lexer state lexbuf}
  | [^'\n']* '\n'        { delim_comment state lexbuf}

and end_lexbuf = parse
  | _     { end_lexbuf lexbuf }
  | eof   { T_EOF (tk lexbuf) }

(*****************************************************************************)
(* space_tok *)
(*****************************************************************************)

and space_tok tok binop state = parse
  | ws+ { binop state lexbuf }
  | nl  { binop state lexbuf }
  | e   { on_def binop (on_local binop tok) state lexbuf}

(*****************************************************************************)
(* uop/binop *)
(*****************************************************************************)

and space_uop uop spc_uop binop state = parse
  (* space before and after is binop (unless at expr_beg) *)
  | ws+ { on_beg spc_uop binop state lexbuf}
  | nl  { on_beg spc_uop binop state lexbuf}
  | e   {match state.S.expr_state with
         | S.Expr_Def
         | S.Expr_Local
         | S.Expr_End -> binop state lexbuf
         | S.Expr_Beg | S.Expr_Mid -> uop state lexbuf
         }

and uop_minus_lit state = parse
  | num { postfix_numeric negate_numeric (Lexing.lexeme lexbuf) state lexbuf}
  | e   { t_uminus state lexbuf}

and uop_plus_lit state = parse
  | num { postfix_numeric Utils.id (Lexing.lexeme lexbuf) state lexbuf}
  | e   { t_uplus state lexbuf}


and postfix_at uop binop state = parse
  | '@' { uop state lexbuf}
  | e   { binop state lexbuf}

(*****************************************************************************)
(* Percent *)
(*****************************************************************************)

and percent state = parse
  | (alpha? as modifier)(string_single_delim as d)
      {S.end_state state; 
       let f = 
         if modifier_is_single modifier
         then non_interp_string d (Buffer.create 31)
         else interp_string_lexer d
       in
         if modifier = "r"
         then regexp_delim ((==)d) ((==)d) (Buffer.create 31) state lexbuf
         else emit_extra (T_USER_BEG(modifier, (tk lexbuf))) f state lexbuf
      }

  | (alpha? as modifier) (['{' '<' '(' '['] as d_start)
      {S.end_state state; 
       let d_end = close_delim d_start in
       let level = ref 0 in
       let at_end d = 
         if d = d_end then
           if !level = 0 then true
           else (decr level; false)
         else
           if d = d_start
           then (incr level; false)
           else false
       in
       let chk d = d == d_start || d == d_end in
       let f = 
         if modifier_is_single modifier
         then non_interp_string d_end (Buffer.create 31)
         else interp_lexer fail_eof at_end chk (Buffer.create 31)
       in
         if modifier = "r"
         then regexp_delim at_end chk (Buffer.create 31) state lexbuf
         else emit_extra (T_USER_BEG(modifier, (tk lexbuf))) f state lexbuf
      }

  | e {t_percent state lexbuf}

(*****************************************************************************)
(* dollar *)
(*****************************************************************************)

and dollar state = parse
  | id as id 
      {S.end_state state; T_GLOBAL_VAR("$"^id, (tk lexbuf)) }
  | ("-" alphanum) as v 
      {S.end_state state; T_BUILTIN_VAR("$"^v, (tk lexbuf)) }
  | ( ['0'-'9']+) as v 
      {S.end_state state; T_BUILTIN_VAR("$"^v, (tk lexbuf))}
  | ( [^'a'-'z''A'-'Z''#'])
      {S.end_state state; T_BUILTIN_VAR("$"^(Lexing.lexeme lexbuf), (tk lexbuf))}

(*****************************************************************************)
(* postfix_numeric *)
(*****************************************************************************)

and postfix_numeric cont start state = parse
  | "b" (['0''1''_']+ as num)
      { S.end_state state; 
        cont (convert_to_base10 ~base:2 num (tk lexbuf))}

  | ('o'|'O') ((num|'_')+ as num)
      { S.end_state state; 
        cont (convert_to_base10 ~base:8 num (tk lexbuf))}

  | "x" ((num|['a'-'f''A'-'F''_'])+ as num)
      { S.end_state state; 
        cont (convert_to_base10 ~base:16 num (tk lexbuf))}

  | (num|'_')* as num
      { S.end_state state;
        if start = "0" 
        then cont (convert_to_base10 ~base:8 num (tk lexbuf))
        else 
          let str = (start ^ num) in
          let num = to_bignum str in
          let tok = 
            if Big_int.is_int_big_int num
            then T_FIXNUM(Big_int.int_of_big_int num, (tk lexbuf))
            else T_BIGNUM(num, (tk lexbuf))
          in cont tok
      }

  | post_rubyfloat
      { S.end_state state;
        let str = (start ^ (Lexing.lexeme lexbuf)) in
        let str = remove_underscores str in
        let tok = T_FLOAT(str, float_of_string (str), (tk lexbuf)) in
          cont tok
      }

(*****************************************************************************)
(* atom *)
(*****************************************************************************)

and atom state = parse
  | ['+' '-' '*' '/' '!' '~' '<' '>' '=' '&' '|' '%' '^' '@']+ 
  | '[' ']' ('='?) (* these can only appear together like this (I think) *)
  | '`' {def_end_state state; 
         T_ATOM((contents_of_str (Lexing.lexeme lexbuf)), (tk lexbuf)) }

  | '$'
      { let str = match dollar state lexbuf with
          | T_GLOBAL_VAR(s,_p) -> s
          | T_BUILTIN_VAR(s,_p) -> s
          | _ -> assert false
        in def_end_state state; 
          T_ATOM(contents_of_str str, (tk lexbuf)) }

  | ('@'* id) '='?
      {def_end_state state; 
       T_ATOM((contents_of_str (Lexing.lexeme lexbuf)), (tk lexbuf)) }
  | '"'  {def_end_state state;
          emit_extra (T_ATOM_BEG (tk lexbuf))
            (interp_string_lexer '"') state lexbuf}

  | '\''  {def_end_state state;
           emit_extra (T_ATOM_BEG (tk lexbuf))
             (non_interp_string '\'' (Buffer.create 31)) state lexbuf}

  | nl  { t_colon state lexbuf}

  | ws
  | e     {t_colon state lexbuf}

(*****************************************************************************)
(* char_code *)
(*****************************************************************************)

and char_code state = parse
  | e {S.beg_state state; char_code_work lexbuf}

and char_code_work = parse
  | [^'\n''\t'] as c {T_FIXNUM(Char.code c, (tk lexbuf))}
  | "\\C-" (['a'-'z''A'-'Z'] as c)
      { T_FIXNUM((Char.code (Char.uppercase_ascii c)) - 64, (tk lexbuf))}
  | "\\M-" (['a'-'z''A'-'Z'] as c)
      { T_FIXNUM((Char.code c) + 128, (tk lexbuf))}
  | "\\M-\\C-" (['a'-'z''A'-'Z'] as c)
  | "\\C-\\M-" (['a'-'z''A'-'Z'] as c)
      { T_FIXNUM((Char.code (Char.uppercase_ascii c)) + 64, (tk lexbuf))}

  | "\\\\" {T_FIXNUM(Char.code '\\', (tk lexbuf))}
  | "\\s" {T_FIXNUM(Char.code ' ', (tk lexbuf))}
  | "\\n" {T_FIXNUM(Char.code '\n', (tk lexbuf))}
  | "\\t" {T_FIXNUM(Char.code '\t', (tk lexbuf))}
  | "\\r" {T_FIXNUM(Char.code '\r', (tk lexbuf))}
  | "\\(" {T_FIXNUM(Char.code '(', (tk lexbuf))}
  | "\\)" {T_FIXNUM(Char.code ')', (tk lexbuf))}

(*****************************************************************************)
(* Strings *)
(*****************************************************************************)

and non_interp_string delim buf state = parse
  | eof   {failwith "eof in string"}

  | '\\'? '\n' { Buffer.add_string buf (Lexing.lexeme lexbuf);
                 non_interp_string delim buf state lexbuf }
  | "\\" _ { Buffer.add_string buf (Lexing.lexeme lexbuf);
             non_interp_string delim buf state lexbuf}
  | _ as c
      {if c == delim 
       then T_SINGLE_STRING(Buffer.contents buf,(tk lexbuf))
       else (Buffer.add_char buf c; non_interp_string delim buf state lexbuf)
      }

and double_string state = parse 
  | e {S.end_state state;
       emit_extra (T_DOUBLE_BEG (tk lexbuf))
         (interp_string_lexer '"') state lexbuf}

and tick_string state = parse 
  | e {S.end_state state;
       emit_extra (T_TICK_BEG (tk lexbuf))
         (interp_string_lexer '`') state lexbuf}

(*****************************************************************************)
(* Regexps *)
(*****************************************************************************)

and regexp state = parse
  | e { regexp_string (Buffer.create 31) state lexbuf }

and regexp_string buf state = parse
  | e { regexp_delim ((==)'/') ((==)'/') buf state lexbuf }

and regexp_delim delim_f escape_f buf state = parse 
  | e {
      let k state lexbuf = 
        pop_lexer state;
        regexp_modifier state lexbuf
      in
        Stack.push k state.S.lexer_stack;
        S.end_state state;
        emit_extra (T_REGEXP_BEG (tk lexbuf))
          (interp_lexer fail_eof delim_f escape_f buf) state lexbuf
    }

and regexp_modifier state = parse
  | (alpha* as modifiers) { T_REGEXP_MOD (modifiers, tk lexbuf(* was nothing*))}

(*****************************************************************************)
(* Heredoc *)
(*****************************************************************************)

and heredoc_header lead_f state = parse
  | '\''([^'\'']+ as delim)'\'' ([^'\n']* nl as rest)
      { let cont str state lexbuf = 
         S.end_state state;
         push_back rest lexbuf;
         T_SINGLE_STRING(str, (tk lexbuf))
       in
          lead_f cont (Buffer.create 31) delim state lexbuf
      }

  | '"'([^'"']+ as delim)'"' ([^'\n']* nl as rest)
  | (id_body+ as delim) ([^'\n']* nl as rest)
      { (* todo: why special adjustments here? *)
        let pos = lexbuf.Lexing.lex_curr_p in
        let cont str state future_lexbuf = 
         let str_lexbuf = Lexing.from_string str in
           str_lexbuf.Lexing.lex_curr_p <- pos;
           push_back rest future_lexbuf;
           interp_heredoc_lexer state str_lexbuf
       in
         emit_extra 
           (T_DOUBLE_BEG (tk lexbuf))
            (lead_f cont (Buffer.create 31) delim) state lexbuf
      }
      
  | e {S.beg_state state; T_LSHFT (tk lexbuf)}

and heredoc_string cont buf delim state = parse (* for <<EOF *)
  | eof {fail_eof (heredoc_string cont buf delim) state lexbuf}
  | ([^'\n']* as tok) ('\n'|eof)
      { if tok = delim
        then cont (Buffer.contents buf) state lexbuf
        else begin 
          Buffer.add_string buf (Lexing.lexeme lexbuf);
          heredoc_string cont buf delim state lexbuf
        end}

and heredoc_string_lead cont buf delim state = parse (* for <<-EOF *)
  | eof {fail_eof (heredoc_string_lead cont buf delim) state lexbuf}
  | [' ''\t']* ([^'\n']* as tok) ('\n'|eof)
      { if tok = delim
        then cont (Buffer.contents buf) state lexbuf
        else begin 
          Buffer.add_string buf (Lexing.lexeme lexbuf);
          heredoc_string_lead cont buf delim state lexbuf
        end}

and interp_heredoc_lexer state = parse
  | e {let f buf state lexbuf = 
         S.end_state state;
         T_INTERP_END(Buffer.contents buf,(tk lexbuf))
       in 
       let nope _  = false in
         interp_lexer f nope nope (Buffer.create 31) state lexbuf
      }


(*****************************************************************************)
(* Interpolated strings *)
(*****************************************************************************)

and interp_string_lexer delim state = parse
  | e { let chk x = x == delim in
        interp_lexer fail_eof chk chk (Buffer.create 31) state lexbuf }

(* tokenize a interpreted string into string / code *)
and interp_lexer do_eof delim_f escape_f buf state = parse
  | eof    { do_eof buf state lexbuf}

  | "\\\n" { interp_lexer do_eof delim_f escape_f buf state lexbuf}
  | '\n'   { Buffer.add_char buf '\n';
             interp_lexer do_eof delim_f escape_f buf state lexbuf}
      
  | "\\" (_ as c)
      {if escape_f c
       then Buffer.add_char buf c
       else Buffer.add_string buf (Lexing.lexeme lexbuf);
       interp_lexer do_eof delim_f escape_f buf state lexbuf}

  | "#{" {S.beg_state state;
          let tok = T_INTERP_STR(Buffer.contents buf,(tk lexbuf)) in
          let k state lexbuf =
            interp_lexer do_eof delim_f escape_f (Buffer.create 31) state lexbuf
          in
            interp_code tok k state lexbuf
         }

  | _ as c
      {if delim_f c 
       then T_INTERP_END(Buffer.contents buf, (tk lexbuf))
       else begin
         Buffer.add_char buf c; 
         interp_lexer do_eof delim_f escape_f buf state lexbuf
       end
      }

and interp_code start cont state = parse
  | e {S.beg_state state;
       let level = ref 0 in
         (* a continuation to read in Ruby tokens until we see an
            unbalanced '}', at which point we abort that continuation
            and restart the [cont] function *)
       let k state _future_lexbuf = 
         match top_lexer state lexbuf with
           | T_LBRACE _ | T_LBRACE_ARG _ as tok -> 
               incr level; tok
                 
           | T_RBRACE _ as tok -> 
               if !level == 0 then begin
                 pop_lexer state; (* abort k *)
                 cont state lexbuf
               end else begin
                 decr level;
                 tok
               end
           | tok -> tok
       in
         Stack.push k state.S.lexer_stack;
         start
      }
