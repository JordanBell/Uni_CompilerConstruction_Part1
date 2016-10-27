open Printf
open Parser_lex
open Parser_types
open Parser_printer
open Parser_evaluator

let filename = Sys.argv.(1)
let num_args = Array.length Sys.argv
let is_verbose = ref false
let store =
{
	decl_funcs = ref [];
  decl_ids   = ref [];
  tbl_refs   = ref (Hashtbl.create 100);
}

let parse_to_program i_str =
	let i_buf = Lexing.from_string i_str in
	try Parser_par.top Parser_lex.read i_buf
	with
		| SyntaxError str -> printf "Parsing Syntax error: %s\n" str; []
		| Parser_par.Error -> printf "Parser failed\n"; []

let myprint a b = if !is_verbose then printf a b else ()

let () =
	(* Go through optional arguments *)
	is_verbose := (num_args >= 3) && (Sys.argv.(2) = "-verbose");

	(* Print the initial message for processing the file *)
	if !is_verbose then
		printf "\n***** Parsing START on file: %s\n" filename
	else
		printf "%s... " filename;

	(* Read in all of the lines *)
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
	myprint "*** Found %d lines\n" num_lines;
	if !is_verbose then printlines !i_lines;
	if num_lines > 0 then
		let lines_as_string = String.concat "\n" !i_lines in
			store.decl_funcs := (parse_to_program lines_as_string);

			if !is_verbose then printf "Result:\n";
			if !is_verbose then print_parse_result !(store.decl_funcs);

			(* Evaluate the program, starting at the main function *)
			try
				let evaluated_expression = eval store (get_func_exp !(store.decl_funcs) "main") in
				print_eval_result store evaluated_expression
			with Failure str -> print_endline str;


	myprint "***** Parsing FINISHED on file: %s\n\n" filename
