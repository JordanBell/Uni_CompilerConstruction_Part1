open Parser_evaluator
open Parser_printer
open Parser
open Printf
open Parser_types
open Instructions
open Hashtbl

(* Configuration *)
let ram = Hashtbl.create 100
let acc = ref 0

(* Instruction execution *)
let op (opc, addr1, addr2) =
  acc := (fun_of_opcode_int opc) (find ram addr1) (find ram addr2)
let st addr = replace ram addr !acc
let ldc n = acc := n

(* Opsets *)
let opset_eval =
{
  on_op = op;
  on_st = st;
  on_ldc = ldc;
}

let interpret = run_with opset_eval []

(* Code for running *)
let filein = Sys.argv.(1)
let num_args = Array.length Sys.argv
let is_verbose = ref false

let get_arg arg_name =
	if num_args >= 3
	then
		(* Search through the array *)
		Array.fold_left (fun x a -> x || a = arg_name) false Sys.argv
	else false

let () =
	(* Go through optional arguments *)
	is_verbose := (get_arg "-verbose") || (get_arg "-v");
  is_verbose := false;
	(* Print the initial message for processing the file *)
	(if !is_verbose then
		printf "\n***** START on file: %s\n" filein);

	(* Read in all of the lines *)
	let i_lines = ref [] in
	let channel = open_in filein in
	try
		while true; do
	 		let this_line = input_line channel in
	    		i_lines := this_line :: !i_lines
	  	done;
	with End_of_file ->
	  	close_in channel;
	  	i_lines := List.rev !i_lines;

	let num_lines = List.length !i_lines in
	(if !is_verbose then (printf "*** Found %d lines\n" num_lines; printlines !i_lines));
	if num_lines > 0 then
		let lines_as_string = String.concat "\n" !i_lines in
		let parsed_program = (parse_to_program lines_as_string) in

		(if !is_verbose then print_parse_result parsed_program);

		try
      (* Perform Interpretation *)
      let eval_start_exp = get_func_exp parsed_program "main" in
      let interpreter_result = interpret eval_start_exp in
      printf "Interpreter result: %d\n" (find ram interpreter_result);
		with Failure str -> print_endline str;

	if !is_verbose then printf "***** Parsing FINISHED on file: %s\n\n" filein
