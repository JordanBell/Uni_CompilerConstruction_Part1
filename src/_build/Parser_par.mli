
(* The type of tokens. *)

type token = 
  | TIMES
  | PLUS
  | MINUS
  | EOF
  | DIVIDE
  | CONST of (int)

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val top: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (int)
