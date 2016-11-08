open Instructions
open Parser_types
open Parser_printer
open Printf
open Parser

let string_of_opcode_instruction opc = match opc with
  | Plus -> "add"
  | Minus -> "sub"
  | Times -> "mul"
  | Divide -> "div"
  | _ -> failwith ("Binary operator instruction not supported: " ^ (string_of_opcode opc))

let code = Buffer.create 100
let codegen_op (opc, addr1, addr2) =
  (string_of_opcode_instruction opc)
  ^ " r" ^ (string_of_int addr1)
  ^ ", r" ^ (string_of_int addr2)
  ^ "\n" |> Buffer.add_string code

let codegen_st addr = "st r" ^ (string_of_int addr)
                      ^ "\n" |> Buffer.add_string code

let codegen_ldc n = "ld " ^ (string_of_int n)
                    ^ "\n" |> Buffer.add_string code

let opset_codegen =
{
  on_op = codegen_op;
  on_st = codegen_st;
  on_ldc = codegen_ldc;
}

let codegen = run_with opset_codegen []

(* Code for running *)
let filein = Sys.argv.(1)
let fileout = Sys.argv.(2)
let num_args = Array.length Sys.argv
let is_verbose = ref false

let get_arg arg_name =
	if num_args >= 4
	then
		(* Search through the array *)
		Array.fold_left (fun x a -> x || a = arg_name) false Sys.argv
	else false

let () =
	(* Go through optional arguments *)
	is_verbose := (get_arg "-verbose") || (get_arg "-v");
  is_verbose := false;
	(* Print the initial message for processing the file *)
	if !is_verbose then
		printf "\n***** START on file: %s\n" filein
	else
		printf "%s... " filein;

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
      (* Perform Code generation *)
      let eval_start_exp = get_func_exp parsed_program "main" in
      let _ = codegen eval_start_exp in
      let out_channel = open_out fileout in
    	Buffer.output_buffer out_channel code;
      printf "Done\n";
		with Failure str -> print_endline str;

	if !is_verbose then (printf "***** Code Generation FINISHED on file: %s\n\n" filein)
