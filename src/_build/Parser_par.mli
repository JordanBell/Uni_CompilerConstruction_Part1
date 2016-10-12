
(* The type of tokens. *)

type token = 
  | TIMES
  | SEQ
  | PLUS
  | PARENTHESIS_OPEN
  | PARENTHESIS_CLOSE
  | OR
  | NOTEQ
  | NOT
  | MINUS
  | LEQ
  | IF
  | GEQ
  | EQUAL
  | EOF
  | ELSE
  | DIVIDE
  | CURLY_OPEN
  | CURLY_CLOSE
  | CONST of (int)
  | ASG
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val top: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parser_types.expression)
