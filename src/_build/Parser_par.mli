
(* The type of tokens. *)

type token = 
  | WHILE
  | TIMES
  | SEQ
  | REF
  | READINT
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
  | EQUAL
  | EOF
  | ELSE
  | DIVIDE
  | DEREF
  | CURLY_OPEN
  | CURLY_CLOSE
  | CONST of (int)
  | COMMA
  | ASG
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val top: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parser_types.program)
