open Printf
open Parser_types
open Parser_lex

(* String Conversions *)
let string_of_opcode = function
	| Plus -> "Plus"
	| Minus -> "Minus"
	| Times -> "Times"
	| Divide -> "Divide"
	| And -> "And"
	| Or -> "Or"
	| Geq -> "Geq"
	| Leq -> "Leq"
	| Equal -> "Equal"
	| Noteq -> "Noteq"
	| _ -> "opcode_str_err"

let string_of_opcode_unary = function	
	| Not -> "Not"
	| _ -> "opcode_str_err"

let string_of_token = function
	| Parser_par.IF -> "if"
	| Parser_par.CONST i -> "Const" ^ (string_of_int i)
	| Parser_par.TIMES -> "Times" 
	| _ -> "token_str_err"

let rec print_expression expr acc = 
	let indent_str = String.make acc '\t' in
	let indent_str_next = String.make (acc+1) '\t' in
	match expr with
		| Operator (op, e, f) -> 
			let opcode_str = string_of_opcode op in
			printf "%sOperator\n%s(\n" indent_str indent_str;
			printf "%s%s" indent_str_next opcode_str; 	printf ",\n";			(* Print opcode *)
			print_expression e (acc+1); 			printf ",\n"; 			(* Print first expr *)
			print_expression f (acc+1); 							(* Print second expr *)
			printf "\n%s)" indent_str

		| Operator_unary (op, e) -> 
			let opcode_str = string_of_opcode_unary op in
			printf "%sOperator_unary\n%s(\n" indent_str indent_str;
			printf "%s%s" indent_str_next opcode_str; 	printf ",\n";			(* Print opcode *)
			print_expression e (acc+1); 							(* Print expr *)
			printf "\n%s)" indent_str

		| If (b, e, f) -> 
			printf "%sIf\n%s(\n" indent_str indent_str;
			print_expression b (acc+1); 		 	printf "\n%sDo\n" indent_str;	(* Print Conditional expr *)
			print_expression e (acc+1); 			printf "\n%sElse\n" indent_str; (* Print do expr *)
			print_expression f (acc+1); 							(* Print else expr *)
			printf "\n%s)" indent_str

		| While (b, e) -> 
			printf "%sWhile\n%s(\n" indent_str indent_str;
			print_expression b (acc+1);			printf ",\n"; 			(* Print first expr *)
			print_expression e (acc+1);							(* Print second expr *)
			printf "\n%s)" indent_str

		| Asg (e, f) ->
			printf "%sAsg\n%s(\n" indent_str indent_str;
			print_expression e (acc+1);			printf ",\n"; 			(* Print first expr *)
			print_expression f (acc+1);							(* Print second expr *)
			printf "\n%s)" indent_str

		| Seq (e, f) ->
			printf "%sSeq\n%s(\n" indent_str indent_str;
			print_expression e (acc+1);			printf ",\n"; 			(* Print first expr *)
			print_expression f (acc+1);							(* Print second expr *)
			printf "\n%s)" indent_str

		| Scope (e) -> 
			printf "%sScope\n%s(\n" indent_str indent_str; 
			print_expression e (acc+1);
			printf "\n%s)" indent_str

		| Printint (e) -> 
			printf "%sPrintint\n%s(\n" indent_str indent_str; 
			print_expression e (acc+1);
			printf "\n%s)" indent_str

		| Readint -> printf "%sReadint" indent_str

		| Const (i) -> printf "%sConst %d" indent_str i
			
		| _ -> printf "Unable to print this kind of expression"

let rec printlines = function
	| h::tl -> printf "%s\n" h; printlines tl
	| _ -> printf ""

let rec print_lex_result = function
	| [] -> printf ""
	| h::tl -> printf "%s" (string_of_token h); print_lex_result tl

let print_parse_result parse_result = printf "*** Result: \n"; print_expression parse_result 0; printf "\n"
