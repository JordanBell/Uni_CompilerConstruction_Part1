open Parser_types
open Parser_printer
open Printf
open Parser

let code = Buffer.create 100
let sp = ref 0
let ifelse_count = ref 0
let while_count = ref 0
let bool_op_count = ref 0

let string_of_opcode_x86 opc = match opc with
  | Plus -> "add"
  | Minus -> "sub"
  | Times -> "imul"
  | Divide -> "idiv"
  | _ -> failwith ("Binary operator instruction not supported: " ^ (string_of_opcode opc))

let codegenx86_op opc =
  match opc with
  | Equal | Noteq | Geq | Leq ->
    let s_lbl_true = ".BOP" ^ (string_of_int !bool_op_count) in
    let s_lbl_continue = ".CONT_BOP" ^ (string_of_int !bool_op_count) in
    "\tpop\t%rax\n" ^
    "\tpop\t%rbx\n" ^
    "\tsub\t%rax, %rbx\n" ^
    "\tmov\t$0, %rax\n" ^
    "\tcmp\t%rax, %rbx\n" |> Buffer.add_string code;

    (match opc with
      | Noteq -> "\tjne\t" ^ s_lbl_true ^ "\n" |> Buffer.add_string code;
      | Equal -> "\tje\t" ^ s_lbl_true ^ "\n" |> Buffer.add_string code;
      | Geq -> "\tjge\t" ^ s_lbl_true ^ "\n" |> Buffer.add_string code;
      | Leq -> "\tjle\t" ^ s_lbl_true ^ "\n" |> Buffer.add_string code;
      | _ -> failwith "opc mismatch: check the match condition this is nested in"
    );

    (* If FALSE *)
    "\tpush\t$0\n" ^
    ("\tjmp\t" ^ s_lbl_continue ^ "\n") |> Buffer.add_string code;

    (* If TRUE *)
    s_lbl_true ^ ":\n" ^
    "\tpush\t$1\n" |> Buffer.add_string code;

    (* Continue *)
    s_lbl_continue ^ ":\n" |> Buffer.add_string code;

    (* Increment Bool-op count *)
    bool_op_count := !bool_op_count + 1
  | _ ->
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

  | Identifier (id_str) | Deref (Identifier (id_str)) ->
    let addr = List.assoc id_str symt in
    codegenx86_id (addr);
    sp := !sp + 1

  | Let (x, e1, e2) | New (x, e1, e2) ->
    frec e1;
    codegenx86 ((x, !sp) :: symt) e2;
    codegenx86_let ()

  | Asg (Identifier (id_str), e) ->
    frec e;
    let addr = List.assoc id_str symt in
    let str_var_reg = (-16 - 8 * addr |> string_of_int) ^ "(%rbp)" in
    "\t# write offset " ^ (string_of_int addr) ^ "\n" ^
    "\tpop\t%rax\n" ^
    "\tmov\t%rax, " ^ str_var_reg ^ "\n" ^
    "\tpush\t" ^ str_var_reg ^ "\n"
    |> Buffer.add_string code

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

  | If (b, e_iftrue, e_iffalse) ->
    (* Generate code for calculating the condition *)
    frec b;

    let str_label_if_nonzero = ".IE" ^ (string_of_int !ifelse_count) in
    let str_label_continue = ".CONT_IE" ^ (string_of_int !ifelse_count) in
    "\tpop\t%rax\n" ^       (* Move the condition value into rax *)
    "\tmov\t$0, %rbx\n" ^   (* Move 0 into rbx for comparison *)
    "\tcmp\t%rax, %rbx\n" ^ (* Compare the condition to 0 *)
    "\tjne\t" ^ str_label_if_nonzero ^ "\n"
    |> Buffer.add_string code;

    (* If not jumping, just execute the else code (where the condition equals 0, ie FALSE) *)
    frec e_iffalse;
    ("\tjmp\t" ^ str_label_continue ^ "\n") |> Buffer.add_string code;

    str_label_if_nonzero ^ ":\n" |> Buffer.add_string code;
    frec e_iftrue;

    str_label_continue ^ ":\n" |> Buffer.add_string code;
    (* Increase the counter of if statement labels *)
    ifelse_count := !ifelse_count + 1

  | While (e, f) ->
    let str_label_loop = ".W" ^ (string_of_int !while_count) in
    let str_label_continue = ".CONT_W" ^ (string_of_int !while_count) in

    (* Place the loop label at the start, which will intiate a condition check and will skip the main expression if false *)
    str_label_loop ^ ":\n" |> Buffer.add_string code;
    frec e;
    "\tpop\t%rax\n" ^       (* Move the condition value into rax *)
    "\tmov\t$0, %rbx\n" ^   (* Move 0 into rbx for comparison *)
    "\tcmp\t%rax, %rbx\n" ^ (* Compare the condition to 0 *)
    "\tje\t" ^ str_label_continue ^ "\n" (* If the condition is 0, exit the loop and continue *)
    |> Buffer.add_string code;

    (* Generate code for the loop expression *)
    frec f;

    (* Jump to the start of the loop, where we recalculate the condition *)
    "\tjmp\t" ^ str_label_loop ^ "\n" |> Buffer.add_string code;
    str_label_continue   ^ ":\n" |> Buffer.add_string code;

    (* Increase while loop count *)
    while_count := 1 + !while_count

  | Empty -> ()
  | Const_string (str) -> failwith "Const_strings not implemented for running with instruction sets."
  | Asg (_, _) -> failwith "Expression not implemented: Asg (with a non-simple-identifier on the left)"
  | Operator_unary (opcode, e) -> failwith "Expression not implemented: Unary Operator (NOT)"
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
