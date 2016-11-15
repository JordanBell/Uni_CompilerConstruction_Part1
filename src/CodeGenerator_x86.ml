open Parser_types
open Parser_printer
open Printf
open Parser

let code = Buffer.create 100
let sp = ref 0

let string_of_opcode_x86 opc = match opc with
  | Plus -> "add"
  | Minus -> "sub"
  | Times -> "imul"
  | Divide -> "idiv"
  | _ -> failwith ("Binary operator instruction not supported: " ^ (string_of_opcode opc))

let codegenx86_op opc =
  "\tpop\t%rax\n" ^
  "\tpop\t%rbx\n" ^
  "\t" ^ (string_of_opcode_x86 opc) ^ "\t%rax, %rbx\n" ^
  "\tpush\t%rbx\n" |> Buffer.add_string code

let codegenx86_id addr =
  "\t# offset " ^ (string_of_int addr) ^ "\n" ^
  "\tmov\t" ^ (-16 - 8 * addr |> string_of_int) ^ "(%rbp), %rax\n" ^
  "\tpush\t%rax\n"
  |> Buffer.add_string code

let codegenx86_st n =
  "\tpush\t$" ^ (string_of_int n) ^ "\n" |> Buffer.add_string code

let codegenx86_let _ =
  "\tpop\t%rax\n" ^
  "\tpop\t%rbx\n" ^
  "\tpush\t%rax\n"
  |> Buffer.add_string code

let rec codegenx86 symt e_in =
  let frec = codegenx86 symt in
  match e_in with
  | Operator (op, e1, e2) ->
    (match op with
      | Divide ->
        frec e1;
        frec e2;
        "\tpop\t%rbx\n" ^
        "\tpop\t%rax\n" ^
        "\tmov\t$0, %rdx\n" ^
        "\tcqto\n" ^
        "\tidivq\t%rbx\n" ^
        "\tpush\t%rax\n" |> Buffer.add_string code;
        sp := !sp + 1
      | _ ->
        frec e1;
        frec e2;
        codegenx86_op op;
        sp := !sp + 1)

  | Identifier (id_str) ->
    let addr = List.assoc id_str symt in
    codegenx86_id (addr);
    sp := !sp + 1

  | Let (x, e1, e2) ->
    frec e1;
    codegenx86 ((x, !sp) :: symt) e2;
    codegenx86_let ()

  | Seq (e, f) ->
    let _ = frec e in
    frec f

  | Const_int n ->
    codegenx86_st n;
    sp := !sp + 1

  | Const_bool b ->
    (* Run the bool as an int *)
    let b_as_i = (if b then 1 else 0) in
    frec (Const_int (b_as_i))

  | Scope (e) -> frec e

  | Const_string (str) -> failwith "Const_strings not implemented for running with instruction sets."
  | Empty -> failwith "Expression not implemented: Empty"
  | Operator_unary (opcode, e) -> failwith "Expression not implemented: Unary Operator (NOT)"
  | If (b, e, f) -> failwith "Expression not implemented: If"
  | While (e, f) -> failwith "Expression not implemented: While"
  | Asg (e, f) -> failwith "Expression not implemented: Asg"
  | New (str, e, f) -> failwith "Expression type not implemented: New"
  | Printint (e) -> failwith "Expression not implemented: Printint"
  | Deref _ ->	failwith "Expression not implemented: Deref"
  | Ref _ -> failwith "Expression type not implemented: Ref"
  | Application (_, _) -> failwith "Expression type not implemented: Application"
  | Readint -> failwith "Expression type not implemented: Readint"


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
      Buffer.add_string code "\n\n\t# Generated code START\n";
      let _ = codegenx86 [] eval_start_exp in
      Buffer.add_string code "\n\t# Push the top of the stack onto the output register\n";
      Buffer.add_string code ("\tpop\t-4(%rbp)\n" ^
                              "\tmovl\t-4(%rbp), %eax\n" ^
                              "\tmovl\t%eax, %edi\n" ^
                              "\t# Generated code END\n\n");
      let out_channel = open_out fileout in
    	Buffer.output_buffer out_channel code;
		with Failure str -> print_endline str;

	if !is_verbose then (printf "***** Code Generation FINISHED on file: %s\n\n" filein)
