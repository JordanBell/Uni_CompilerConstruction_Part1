
(* The type of tokens. *)

type token = 
  | WHILE
  | TRUE
  | TIMES
  | SEQ
  | REF
  | READINT
  | QUOTE_DOUBLE
  | PRINTINT
  | PLUS
  | PARENTHESIS_OPEN
  | PARENTHESIS_CLOSE
  | OR
  | NOTEQ
  | NOT
  | NEW
  | MINUS
  | LET
  | LEQ
  | IN
  | IF
  | IDENTIFIER of (string)
  | GEQ
  | FALSE
  | EQUAL
  | EOF
  | ELSE
  | DIVIDE
  | DEREF
  | CURLY_OPEN
  | CURLY_CLOSE
  | CONST_INT of (int)
  | COMMA
  | ASG
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val top: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parser_types.program)
