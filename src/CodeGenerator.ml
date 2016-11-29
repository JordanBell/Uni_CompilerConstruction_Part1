open Instructions
open Parser_types
open Parser_printer
open Printf
open Parser

let code = Buffer.create 100

let string_of_opcode_instruction opc = match opc with
  | Plus -> "add"
  | Minus -> "sub"
  | Times -> "mul"
  | Divide -> "div"
  | _ -> failwith ("Binary operator instruction not supported: " ^ (string_of_opcode opc))

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

	let parsed_program = parse_file filein !is_verbose in

  try
    (* Perform Code generation *)
    match parsed_program with (funcdefs, struct_defs) ->
    let eval_start_exp = get_func_exp funcdefs "main" in
    let _ = codegen eval_start_exp in
    let out_channel = open_out fileout in
  	Buffer.output_buffer out_channel code;
    printf "Done\n";
	with Failure str -> print_endline str;
