
(* The type of tokens. *)

type token = 
  | WHILE
  | TRUE
  | TIMES
  | STRUCT
  | STRING_VALUE of (string)
  | SEQ
  | REF
  | READINT
  | PRINT
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
  | INCLUDE
  | IN
  | IF
  | IDENTIFIER of (string)
  | GEQ
  | FALSE
  | EQUAL
  | EOF
  | ELSE
  | DOT
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

val top: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Parser_types.program * (string list))
