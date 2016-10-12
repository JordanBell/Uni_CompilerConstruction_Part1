{
open Parser_par
exception SyntaxError of string
}

let int = ['0'-'9'] ['0'-'9']*
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read =
	parse
	| white 	{ read lexbuf }
	| newline 	{ read lexbuf }
	| int 		{ CONST (int_of_string (Lexing.lexeme lexbuf)) }
	| '+' 		{ PLUS }
	| '-' 		{ MINUS }
	| '*' 		{ TIMES }
	| '/' 		{ DIVIDE }
	(*| '='		{ ASG }
	| "<="		{ LEQ }
	| ">="		{ GEQ }
	| "=="		{ EQUAL }
	| "!="		{ NOTEQ }*)
	| _ 		{ raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
	| eof 		{ EOF }
