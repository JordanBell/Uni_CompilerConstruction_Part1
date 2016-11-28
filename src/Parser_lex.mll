{
open Parser_par
exception SyntaxError of string
}

let int = ['0'-'9'] ['0'-'9']*
let string_value = '"' [^'\n']* '"'
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let identifier_string = (['a'-'z'] | ['A'-'Z'] | '_') (['a'-'z'] | ['A'-'Z'] | '_' | ['0'-'9'])*
let comment = "//" [^'\n']* newline

rule read =
	parse
	(* Skip-cases *)
	| white 		{ read lexbuf }
	| newline 		{ read lexbuf }
	| comment 		{ read lexbuf }

	(* Arithmetic *)
	| int 			{ CONST_INT (int_of_string (Lexing.lexeme lexbuf)) }
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
	| "print"		{ PRINT }
	| "let"			{ LET }
	| "new"			{ NEW }
	| "in"			{ IN }
	| "true" 		{ TRUE }
	| "false"		{ FALSE }
	| "struct"	{ STRUCT }

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
	| '.'			{ DOT }
	| ';'			{ SEQ }
	| ','			{ COMMA }
	| '&'			{ REF }
	| '~'			{ DEREF }
	| identifier_string	{ IDENTIFIER (Lexing.lexeme lexbuf) }
	| string_value	{
			let full_str = Lexing.lexeme lexbuf in
			let str_len = String.length full_str in
			STRING_VALUE (String.sub full_str 1 (str_len-2)) }

	(* Meta *)
	| _ 			{ raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
	| eof 			{ EOF }
