open Printf
open Parser_lex
open Parser_types
open Parser_printer

let filename = Sys.argv.(1)

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
