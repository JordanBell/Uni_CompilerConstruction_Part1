open Printf
open Parser_par
open Parser_lex
open Parser_types

let rec count_exp e = match e with
	| Operator (op, e, f) -> 1 + count_exp e + count_exp f
	| Operator_unary (op, e) -> 1 + count_exp e
	| If (b, e, Empty) -> 1 + count_exp e
	| If (b, e, f) -> 1 + count_exp e + count_exp f
	| While (b, e) -> 1 + count_exp e
	| Asg (e, f) -> 1 + count_exp e + count_exp f
	| Seq (e, f) -> count_exp e + count_exp f
	| Application (e, fs) -> count_exp e (* TODO This is a bit more complicated. *)
	| Let (str, e, f) -> 1 + count_exp e + count_exp f
	| New (str, e, f) -> 1 + count_exp e + count_exp f
	| Scope (e) -> count_exp e
	| Deref (e) -> 1 + count_exp e
	| Ref (e) -> 1 + count_exp e
	| Printint (e) -> 1 + count_exp e
	| Readint ->  1
	| Const_int (_) | Const_bool (_) | Const_string (_) -> 1
	| Identifier (str) -> 1
	| Empty -> 0

let parse_to_program i_str =
	let i_buf = Lexing.from_string i_str in
	try Parser_par.top Parser_lex.read i_buf
	with
		| SyntaxError str -> printf "Parsing Syntax error: %s\n" str; []
		| Parser_par.Error -> printf "Parser failed\n"; []
