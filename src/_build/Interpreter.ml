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

	let parsed_program = (parse_file filein !is_verbose) in

	try
    (* Perform Interpretation *)
    match parsed_program with (funcdefs, struct_defs) ->
    let eval_start_exp = get_func_exp funcdefs "main" in
    let interpreter_result = interpret eval_start_exp in
    printf "Interpreter result: %d\n" (find ram interpreter_result);
	with Failure str -> print_endline str;
