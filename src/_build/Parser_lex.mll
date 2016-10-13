{
open Parser_par
exception SyntaxError of string
}

let int = ['0'-'9'] ['0'-'9']*
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let identifier_string = (['a'-'z'] | ['A'-'Z'] | '_')+

rule read =
	parse
	(* Skip-cases *)
	| white 		{ read lexbuf }
	| newline 		{ read lexbuf }

	(* Arithmetic *)
	| int 			{ CONST (int_of_string (Lexing.lexeme lexbuf)) }
	| '+' 			{ PLUS }
	| '-' 			{ MINUS }
	| '*' 			{ TIMES }
	| '/' 			{ DIVIDE }

	(* Scope *)
	| '{'			{ CURLY_OPEN }
	| '}'			{ CURLY_CLOSE }
	| '('			{ PARENTHESIS_OPEN }
	| ')'			{ PARENTHESIS_CLOSE }

	(* Keywords *) 
	| "if"			{ IF }
	| "while"		{ WHILE }
	| "else"		{ ELSE }
	| "read_int"		{ READINT }
	| "print_int"		{ PRINTINT }
(*	| "let"			{ LET }
	| "new"			{ NEW }
	| "in"			{ IN }*)
	| "int"			{ TYPE_INT }

	(* Bool ops *)
	| "<="			{ LEQ }
	| ">="			{ GEQ }
	| "=="			{ EQUAL }
	| "!="			{ NOTEQ }
	| "&&"			{ AND }
	| "||"			{ OR }
	| '!'			{ NOT }

	(* Misc *)
	| '='			{ ASG }
	| ';'			{ SEQ }
	| ','			{ COMMA }
	| '~'			{ DEREF }
	| identifier_string	{ IDENTIFIER (Lexing.lexeme lexbuf) }

	(* Meta *)
	| _ 			{ raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
	| eof 			{ EOF }


