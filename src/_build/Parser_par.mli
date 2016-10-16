
(* The type of tokens. *)

type token = 
  | WHILE
  | TYPE_INT
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
  | MINUS
  | LEQ
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
