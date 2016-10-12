open Printf
open Parser_lex
open Parser_types

let filename = Sys.argv.(1)

let string_of_opcode = function
	| Plus -> "Plus"
	| Minus -> "Minus"
	| Times -> "Times"
	| Divide -> "Divide"
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
		| If (b, e, f) -> 
			printf "%sIf\n%s(\n" indent_str indent_str;
			print_expression b (acc+1); 		 	printf "\n%sDo\n" indent_str;	(* Print Conditional expr *)
			print_expression e (acc+1); 			printf "\n%sElse\n" indent_str; (* Print do expr *)
			print_expression f (acc+1); 							(* Print else expr *)
			printf "\n%s)" indent_str
		| Const (i) -> printf "%sConst %d" indent_str i
		| Scope (e) -> 
			printf "%sScope\n%s(\n" indent_str indent_str; 
			print_expression e (acc+1);
			printf "\n%s)" indent_str
		| Asg (e, f) ->
			printf "%sAsg\n%s(\n" indent_str indent_str;
			print_expression e (acc+1);			printf ",\n"; 			(* Print first expr *)
			print_expression e (acc+1);							(* Print second expr *)
			printf "\n%s)" indent_str
		| _ -> printf "Unable to print this kind of expression"

let rec printlines = function
	| h::tl -> printf "%s\n" h; printlines tl
	| _ -> printf ""

let rec print_lex_result = function
	| [] -> printf ""
	| h::tl -> printf "%s" (string_of_token h); print_lex_result tl

let print_parse_result parse_result = printf "*** Result: \n"; print_expression parse_result 0; printf "\n"

let do_parse i_str = 
	let i_buf = Lexing.from_string i_str in
	let parse_result = Parser_par.top Parser_lex.read i_buf in
	print_parse_result parse_result (* Print the result *)

let () = 
	printf "\n***** Parsing START on file: %s\n" filename;
	let i_lines = ref [] in
	let channel = open_in filename in
	try
	  while true; do
	    let this_line = input_line channel in
	    i_lines := this_line :: !i_lines
	  done;
	with End_of_file ->
	  close_in channel;
	  i_lines := List.rev !i_lines;
	let num_lines = List.length !i_lines in
	printf "*** Found %d lines\n" num_lines;
	printlines !i_lines;
	if num_lines > 0 then
		let lines_as_string = String.concat "\n" !i_lines in
		do_parse lines_as_string;;
	printf "***** Parsing FINISHED on file: %s\n\n" filename
