{
open Parser_par
exception SyntaxError of string
}

let int = ['0'-'9'] ['0'-'9']*
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read =
	parse
	(* Skip-cases *)
	| white 	{ read lexbuf }
	| newline 	{ read lexbuf }

	(* Arithmetic *)
	| int 		{ CONST (int_of_string (Lexing.lexeme lexbuf)) }
	| '+' 		{ PLUS }
	| '-' 		{ MINUS }
	| '*' 		{ TIMES }
	| '/' 		{ DIVIDE }

	(* Scope *)
	| '{'		{ CURLY_OPEN }
	| '}'		{ CURLY_CLOSE }
	| '('		{ PARENTHESIS_OPEN }
	| ')'		{ PARENTHESIS_CLOSE }

	(* Keywords *) 
	| "if"		{ IF }
	| "else"	{ ELSE }

	(* Bool ops *)
	| "<="		{ LEQ }
	| ">="		{ GEQ }
	| "=="		{ EQUAL }
	| "!="		{ NOTEQ }
	| "&&"		{ AND }
	| "||"		{ OR }
	| '!'		{ NOT }

	(* Misc *)
	| '='		{ ASG }
	| ';'		{ SEQ }

	(* Meta *)
	| _ 		{ raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
	| eof 		{ EOF }
