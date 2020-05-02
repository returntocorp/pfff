{
(* Joust: a Java lexer, parser, and pretty-printer written in OCaml
 *  Copyright (C) 2001  Eric C. Cooper <ecc@cmu.edu>
 *  Released under the GNU General Public License 
 * 
 * ocamllex lexer for Java
 * 
 * Attempts to conform to:
 * The Java Language Specification Second Edition
 * - James Gosling, Bill Joy, Guy Steele, Gilad Bracha 
 *
 * Extended by Yoann Padioleau to support more recent versions of Java.
 * Copyright (C) 2011 Facebook
 * Copyright (C) 2020 r2c
 *)

open Ast_generic (* for arithmetic operators *)
open Parser_java
module Flag = Flag_parsing

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* shortcuts *)
let tok = Lexing.lexeme
let tokinfo = Parse_info.tokinfo
let error = Parse_info.lexical_error

(* ---------------------------------------------------------------------- *)
(* Keywords *)
(* ---------------------------------------------------------------------- *)
let primitive_type t = (t, (fun ii -> PRIMITIVE_TYPE (t, ii)))

let keyword_table = Common.hash_of_list [
  "if", (fun ii -> IF ii);
  "else", (fun ii -> ELSE ii);

  "while", (fun ii -> WHILE ii);
  "do", (fun ii -> DO ii);
  "for", (fun ii -> FOR ii);

  "return", (fun ii -> RETURN ii);
  "break", (fun ii -> BREAK ii);
  "continue", (fun ii -> CONTINUE ii);

  "switch", (fun ii -> SWITCH ii);
  "case", (fun ii -> CASE ii);
  (* javaext: now also use for interface default implementation in 1.? *)
  "default", (fun ii -> DEFAULT ii);

  "goto", (fun ii -> GOTO ii);

  "try", (fun ii -> TRY ii);
  "catch", (fun ii -> CATCH ii);
  "finally", (fun ii -> FINALLY ii);
  "throw", (fun ii -> THROW ii);

  "synchronized", (fun ii -> SYNCHRONIZED ii);


  "true", (fun ii -> TRUE ii);
  "false", (fun ii -> FALSE ii);
  "null", (fun ii -> NULL ii);

  "void", (fun ii -> VOID ii);

  primitive_type "boolean";
  primitive_type "byte";
  primitive_type "char";
  primitive_type "short";
  primitive_type "int";
  primitive_type "long";
  primitive_type "float";
  primitive_type "double";

  "class", (fun ii -> CLASS ii);
  "interface", (fun ii -> INTERFACE ii);
  "extends", (fun ii -> EXTENDS ii);
  "implements", (fun ii -> IMPLEMENTS ii);

  "this", (fun ii -> THIS ii);
  "super", (fun ii -> SUPER ii);
  "new", (fun ii -> NEW ii);
  "instanceof", (fun ii -> INSTANCEOF ii);

  "abstract", (fun ii -> ABSTRACT ii);
  "final", (fun ii -> FINAL ii);

  "private", (fun ii -> PRIVATE ii);
  "protected", (fun ii -> PROTECTED ii);
  "public", (fun ii -> PUBLIC ii);

  "const", (fun ii -> CONST ii);

  "native", (fun ii -> NATIVE ii);
  "static", (fun ii -> STATIC ii);
  "strictfp", (fun ii -> STRICTFP ii);
  "transient", (fun ii -> TRANSIENT ii);
  "volatile", (fun ii -> VOLATILE ii);

  "throws", (fun ii -> THROWS ii);

  "package", (fun ii -> PACKAGE ii);
  "import", (fun ii -> IMPORT ii);

  (* javaext: 1.4 *)
  "assert", (fun ii -> ASSERT ii);
  (* javaext: 1.? *)
  "enum", (fun ii -> ENUM ii);
  (* javaext: 1.? *)
  (*  "var", (fun ii -> VAR ii); REGRESSIONS *)
]

}
(*****************************************************************************)
(* Regexps aliases *)
(*****************************************************************************)
let LF = '\n'  (* newline *)
let CR = '\r'  (* return *)

let LineTerminator = LF | CR | CR LF
let InputCharacter = [^ '\r' '\n']

let SUB = '\026' (* control-Z *) (* decimal *)

let SP = ' '     (* space *)
let HT = '\t'    (* horizontal tab *)
let FF = '\012'  (* form feed *) (* decimal *)

let _WhiteSpace = SP | HT | FF (* | LineTerminator -- handled separately *)

(* let TraditionalComment = "/*" ([^ '*'] | '*' [^ '/'])* "*/" *)
let EndOfLineComment = "//" InputCharacter* LineTerminator
(* let Comment = TraditionalComment | EndOfLineComment *)

let Letter = ['A'-'Z' 'a'-'z' '_' '$']
let Digit = ['0'-'9']

let Identifier = Letter (Letter | Digit)*

let NonZeroDigit = ['1'-'9']
let HexDigit = ['0'-'9' 'a'-'f' 'A'-'F']
let OctalDigit = ['0'-'7']
let BinaryDigit = ['0'-'1']

(* javaext: underscore in numbers *)
let IntegerTypeSuffix = ['l' 'L']
let Underscores = '_' '_'*

let DigitOrUnderscore = Digit | '_'
let DigitsAndUnderscores = DigitOrUnderscore DigitOrUnderscore*
let Digits = Digit | Digit DigitsAndUnderscores? Digit

let DecimalNumeral = 
  "0"
| NonZeroDigit Digits?
| NonZeroDigit Underscores Digits

let DecimalIntegerLiteral = DecimalNumeral IntegerTypeSuffix?

let HexDigitOrUndercore = HexDigit | '_'
let HexDigitsAndUnderscores = HexDigitOrUndercore HexDigitOrUndercore*
let HexDigits = HexDigit | HexDigit HexDigitsAndUnderscores? HexDigit
let HexNumeral = ("0x" | "0X") HexDigits
let HexIntegerLiteral = HexNumeral IntegerTypeSuffix?

let OctalDigitOrUnderscore = OctalDigit | '_'
let OctalDigitsAndUnderscores = OctalDigitOrUnderscore OctalDigitOrUnderscore*
let OctalDigits = OctalDigit | OctalDigit OctalDigitsAndUnderscores? OctalDigit
let OctalNumeral = "0" (OctalDigits | Underscores OctalDigits)
let OctalIntegerLiteral = OctalNumeral IntegerTypeSuffix?

let BinaryDigitOrUnderscore = BinaryDigit | '_'
let BinaryDigitsAndUnderscores = BinaryDigitOrUnderscore BinaryDigitOrUnderscore*
let BinaryDigits = BinaryDigit | BinaryDigit BinaryDigitsAndUnderscores? BinaryDigit
let BinaryNumeral = ("0b" | "0B") BinaryDigits
let BinaryIntegerLiteral = BinaryNumeral IntegerTypeSuffix?

let IntegerLiteral =
  DecimalIntegerLiteral
| HexIntegerLiteral
| OctalIntegerLiteral
(* javaext: ? *)
| BinaryIntegerLiteral

let ExponentPart = ['e' 'E'] ['+' '-']? Digit+

let FloatTypeSuffix = ['f' 'F' 'd' 'D']

let FloatingPointLiteral =
  (Digit+ '.' Digit* | '.' Digit+) ExponentPart? FloatTypeSuffix?
| Digit+ (ExponentPart FloatTypeSuffix? | ExponentPart? FloatTypeSuffix)

let BooleanLiteral = "true" | "false"

let OctalEscape = '\\' ['0'-'3']? OctalDigit? OctalDigit

(* Not in spec -- added because we don't handle Unicode elsewhere. *)

let UnicodeEscape = "\\u" HexDigit HexDigit HexDigit HexDigit

let EscapeSequence =
  '\\' ['b' 't' 'n' 'f' 'r' '"' '\'' '\\']
| OctalEscape
| UnicodeEscape

(* ugly: hardcoded stuff for fbandroid, error in SoundexTest.java *)
let UnicodeX = "�"

let SingleCharacter = [^ '\'' '\\' '\n' '\r']
let CharacterLiteral = '\'' (SingleCharacter | EscapeSequence | UnicodeX ) '\''


let StringCharacter = [^ '"' '\\' '\n' '\r']
(* used inline later *)
let StringLiteral = '"' (StringCharacter | EscapeSequence)* '"'

let NullLiteral = "null"

let _Literal =
  IntegerLiteral
| FloatingPointLiteral
| CharacterLiteral
| StringLiteral
| BooleanLiteral
| NullLiteral

(* Assignment operators, except '=', from section 3.12 *)

let _AssignmentOperator =
  ('+' | '-' | '*' | '/' | '&' | '|' | '^' | '%' | "<<" | ">>" | ">>>") '='


let newline = '\n'

(*****************************************************************************)
(* Token rule *)
(*****************************************************************************)
rule token = parse

  (* ----------------------------------------------------------------------- *)
  (* spacing/comments *)
  (* ----------------------------------------------------------------------- *)
  | [' ' '\t' '\r' '\011' '\012' ]+  { TCommentSpace (tokinfo lexbuf) }

  | newline { TCommentNewline (tokinfo lexbuf) }

  | "/*"
    { 
      let info = tokinfo lexbuf in 
      let com = comment lexbuf in
      TComment(info |> Parse_info.tok_add_s com) 
    }
  (* don't keep the trailing \n; it will be in another token *)
  | "//" InputCharacter* 
   { TComment(tokinfo lexbuf) }


  (* ----------------------------------------------------------------------- *)
  (* Constant *)
  (* ----------------------------------------------------------------------- *)

  | IntegerLiteral       { TInt (tok lexbuf, tokinfo lexbuf) }
  | FloatingPointLiteral { TFloat (tok lexbuf, tokinfo lexbuf) }
  | CharacterLiteral     { TChar (tok lexbuf, tokinfo lexbuf) }
  | '"' ( (StringCharacter | EscapeSequence)* as s) '"'
   { TString (s, tokinfo lexbuf) }
  (* bool and null literals are keywords, see below *)

  (* ----------------------------------------------------------------------- *)
  (* Keywords and ident (must be after "true"|"false" above) *)
  (* ----------------------------------------------------------------------- *)
  | Identifier
    { 
      let info = tokinfo lexbuf in
      let s = tok lexbuf in
   
      match Common2.optionise (fun () -> Hashtbl.find keyword_table s) with
      | Some f -> f info
      | None -> IDENTIFIER (s, info)
    }

  (* sgrep-ext: *)
  | '$' Identifier 
    { let s = tok lexbuf in
      if not !Flag_parsing.sgrep_mode
      then error ("identifier with dollar: "  ^ s) lexbuf;
      IDENTIFIER (s, tokinfo lexbuf)
    }

  (* ----------------------------------------------------------------------- *)
  (* Symbols *)
  (* ----------------------------------------------------------------------- *)

  | '('  { LP(tokinfo lexbuf) } | ')'  { RP(tokinfo lexbuf) }
  | '{'  { LC(tokinfo lexbuf) } | '}'  { RC(tokinfo lexbuf) }
  | '['  { LB(tokinfo lexbuf) } | ']'  { RB(tokinfo lexbuf) }
  | ';'  { SM(tokinfo lexbuf) }
  | ','  { CM(tokinfo lexbuf) }
  | '.'  { DOT(tokinfo lexbuf) }
  
  (* pad: to avoid some conflicts *)
  | "[]"  { LB_RB(tokinfo lexbuf) }
  
  | "="  { EQ(tokinfo lexbuf) }
  (* relational operator also now used for generics, can be transformed in LT2 *)
  | "<"  { LT(tokinfo lexbuf) } | ">"  { GT(tokinfo lexbuf) }
  | "!"  { NOT(tokinfo lexbuf) }
  | "~"  { COMPL(tokinfo lexbuf) }
  | "?"  { COND(tokinfo lexbuf) }
  | ":"  { COLON(tokinfo lexbuf) }
  | "=="  { EQ_EQ(tokinfo lexbuf) }
  | "<="  { LE(tokinfo lexbuf) } | ">="  { GE(tokinfo lexbuf) }
  | "!="  { NOT_EQ(tokinfo lexbuf) }
  | "&&"  { AND_AND(tokinfo lexbuf) } | "||"  { OR_OR(tokinfo lexbuf) }
  | "++"  { INCR(tokinfo lexbuf) } | "--"  { DECR(tokinfo lexbuf) }
  | "+"  { PLUS(tokinfo lexbuf) } | "-"  { MINUS(tokinfo lexbuf) }
  | "*"  { TIMES(tokinfo lexbuf) } | "/"  { DIV(tokinfo lexbuf) }
  | "&"  { AND(tokinfo lexbuf) } 
  (* javaext: also used inside catch for list of possible exn *)
  | "|"  { OR(tokinfo lexbuf) }
  | "^"  { XOR(tokinfo lexbuf) }
  | "%"  { MOD(tokinfo lexbuf) }
  | "<<"  { LS(tokinfo lexbuf) } 
  (* this may be split in two tokens in fix_tokens_java.ml *)
  | ">>"  { SRS(tokinfo lexbuf) }
  | ">>>"  { URS(tokinfo lexbuf) }
  (* javaext: lambdas *)
  | "->" { ARROW (tokinfo lexbuf) }
  (* javaext: qualified method *)
  | "::" { COLONCOLON (tokinfo lexbuf) }
  
  (* ext: annotations *)
  | "@" { AT(tokinfo lexbuf) }
  (* regular feature of Java for params and sgrep-ext: *)
  | "..."  { DOTS(tokinfo lexbuf) }
  (* sgrep-ext: *)
  | "<..."  { Flag_parsing.sgrep_guard (LDots (tokinfo lexbuf)) }
  | "...>"  { Flag_parsing.sgrep_guard (RDots (tokinfo lexbuf)) }
  
  | "+="  { OPERATOR_EQ (Plus, tokinfo lexbuf) }
  | "-="  { OPERATOR_EQ (Minus, tokinfo lexbuf) }
  | "*="  { OPERATOR_EQ (Mult, tokinfo lexbuf) }
  | "/="  { OPERATOR_EQ (Div, tokinfo lexbuf) }
  | "%="  { OPERATOR_EQ (Mod, tokinfo lexbuf) }
  | "&="  { OPERATOR_EQ (BitAnd, tokinfo lexbuf) }
  | "|="  { OPERATOR_EQ (BitOr, tokinfo lexbuf) }
  | "^="  { OPERATOR_EQ (BitXor, tokinfo lexbuf) }
  | "<<=" { OPERATOR_EQ (LSL, tokinfo lexbuf) }
  | ">>=" { OPERATOR_EQ (LSR, tokinfo lexbuf) }
  | ">>>="{ OPERATOR_EQ (ASR, tokinfo lexbuf) }
  
  | SUB? eof { EOF (tokinfo lexbuf |> Parse_info.rewrap_str "") }
  
  | _ { 
  error ("unrecognised symbol, in token rule:"^tok lexbuf) lexbuf;
  TUnknown (tokinfo lexbuf)
  }

(*****************************************************************************)
(* Comments *)
(*****************************************************************************)

(* less: allow only char-'*' ? *)
and comment = parse
  | "*/"     { tok lexbuf }
  (* noteopti: *)
  | [^ '*']+ { let s = tok lexbuf in s ^ comment lexbuf }
  | [ '*']   { let s = tok lexbuf in s ^ comment lexbuf }
  | eof  { error ("Unterminated_comment") lexbuf;  ""  }
  | _  {
    let s = tok lexbuf in
    error ("unrecognised symbol in comment:"^s) lexbuf;
    s ^ comment lexbuf
  }
