open Parser_types
open Parser_printer
open Printf
open Parser
open String

(* String Buffers *)
let code_functions = Buffer.create 100
let code = Buffer.create 100

(* Counters *)
let sp = ref 0
let ifelse_count = ref 0
let while_count = ref 0
let bool_op_count = ref 0

let stack_offset_default = -16
let stack_offset = ref stack_offset_default

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
    "\tpush\t$0\n" ^ (* Push a value of FALSE *)
    "\tjmp\t" ^ s_lbl_continue ^ "\n" |> Buffer.add_string code;

    (* If TRUE *)
    s_lbl_true ^ ":\n" ^
    "\tpush\t$1\n" |> Buffer.add_string code; (* Push a value of TRUE *)

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
  "\tmov\t" ^ (!stack_offset - (8 * addr) |> string_of_int) ^ "(%rbp), %rax\n" ^
  "\tpush\t%rax\n"
  |> Buffer.add_string code

let codegenx86_st n =
  "\tpush\t$" ^ (string_of_int n) ^ "\n" |> Buffer.add_string code

(* let codegenx86_st_struct s_data =
  "\tpush\t$" ^ (string_of_int n) ^ "\n" |> Buffer.add_string code *)

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
        "### Incrementing Stack Pointer: " ^ (string_of_int (!sp + 1)) ^ "\n" |> Buffer.add_string code;
        sp := !sp + 1
      | _ ->
        frec e1;
        frec e2;
        codegenx86_op op;
        "### Incrementing Stack Pointer: " ^ (string_of_int (!sp + 1)) ^ "\n" |> Buffer.add_string code;
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

  | Const_struct (s_data) ->
    failwith "Unsupported expression: Const_struct"
    (* codegenx86_st_struct s_data;
    sp := !sp + (Hashtbl.length s_data); *)

  | Memaccess (_, _) -> failwith "Unsupported expression: Memaccess"


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

    (* Increase the counter of if statement labels *)
    ifelse_count := !ifelse_count + 1;

    (* If not jumping, just execute the else code (where the condition equals 0, ie FALSE) *)
    frec e_iffalse;
    ("\tjmp\t" ^ str_label_continue ^ "\n") |> Buffer.add_string code;

    str_label_if_nonzero ^ ":\n" |> Buffer.add_string code;
    frec e_iftrue;

    str_label_continue ^ ":\n" |> Buffer.add_string code

  | While (e, f) ->
    let str_label_loop = ".W" ^ (string_of_int !while_count) in
    let str_label_continue = ".CONT_W" ^ (string_of_int !while_count) in

    (* Increase while loop count *)
    while_count := 1 + !while_count;

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
    str_label_continue   ^ ":\n" |> Buffer.add_string code

  | Application (Identifier (str_func), args) ->
    (* Push all of the args onto the stack *)
    let rec frec_all = function
      | h::tl -> frec h; frec_all tl
      | _ -> ()
    in
    frec_all args;
    "\tcall\t" ^ str_func ^ "\n" ^
    "\tpush\t%rax\n"
    |> Buffer.add_string code

  | Empty -> ()
  | Const_string (str) -> failwith "Const_strings not implemented for running with instruction sets."
  | Asg (_, _) -> failwith "Expression not implemented: Asg (with a non-simple-identifier on the left)"
  | Operator_unary (opcode, e) -> failwith "Expression not implemented: Unary Operator (NOT)"
  | Application (_, _) -> failwith "Expression not implemented: Assignment without explicit function name to be called"
  | Printint (e) -> failwith "Expression not implemented: Printint"
  | Deref _ ->	failwith "Expression not implemented: Deref"
  | Ref _ -> failwith "Expression type not implemented: Ref"
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

let generate_function f = match f with Myfunc (str_func, args, e) ->
    if (String.compare str_func "main" != 0)
    then
    (
      (* Function name Label stack pointer operations *)
      str_func ^ ":\n" ^
      "\tpushq\t%rbp\n" ^
      "\tmovq %rsp, %rbp\n"
      |> Buffer.add_string code;

      (* Build the symbol table from the args *)
      let symt = ref [] in
      stack_offset := 8 + (List.length args) * 8; (* Note: The constant 8 at the start accounts for the pushed rbp value *)
      sp := 0;
      List.iter (fun arg_name -> symt := (arg_name, !sp)::!symt; sp := !sp + 1) args;
      codegenx86 (!symt) e; (* Generate function code *)
      stack_offset := stack_offset_default;
      sp := 0;

      (* Footer stack pointer and return operations *)
      "\tpopq\t%rax\n" ^
      "\tmovq %rbp, %rsp\n" ^ (* Move the stack pointer to the function-relative base position; leave the local variables in the red zone *)
      "\tpopq\t%rbp\n" ^
      "\tret\n"
      |> Buffer.add_string code
    )

let generate_functions parsed_program fileout_funcs =
  Buffer.clear code; (* Clear the buffer *)
  List.iter generate_function parsed_program;

  (* Write the code buffer to the functions' output file *)
  let out_channel = open_out fileout_funcs in
  Buffer.output_buffer out_channel code;
  Buffer.clear code (* Clear the buffer to be re-used later *)

let () =
	(* Go through optional arguments *)
	is_verbose := (get_arg "-verbose") || (get_arg "-v");

	let parsed_program = (parse_file filein !is_verbose) in

  try
  match parsed_program with (funcdefs, struct_defs) ->

    let file_frag_dir = "test_cases/CodeGeneration/GeneratedCode/x86/" in
    let file_frag_1 = file_frag_dir ^ "1.x86frag" in
    (* x86 function code will be concatenated here *)
    let file_frag_2 = file_frag_dir ^ "3.x86frag" in
    (* main function code will be concatenated here *)
    let file_frag_3 = file_frag_dir ^ "5.x86frag" in

    Buffer.clear code; (* Clear the buffer *)
    (* Add function code for each function definition in the parsed program *)
    List.iter generate_function funcdefs;

    (* Copy over the generated code to a new function, so the general purpose code buffer can be reused. *)
    let code_funcs = Buffer.create 100 in
    Buffer.add_bytes code_funcs (Buffer.to_bytes code);
    Buffer.clear code; (* Clear the buffer to be re-used for the main code *)

    (* Perform main Code generation *)
    let eval_start_exp = get_func_exp funcdefs "main" in
    Buffer.add_string code "\n\n\t# Generated code START\n";
    let _ = codegenx86 [] eval_start_exp in
    Buffer.add_string code "\n\t# Push the top of the stack onto the output register\n";
    Buffer.add_string code ("\tpop\t-4(%rbp)\n" ^
                            "\tmovl\t-4(%rbp), %eax\n" ^
                            "\tmovl\t%eax, %edi\n" ^
                            "\t# Generated code END\n\n");

    (* Build the buffer for the entire x86 code output file (with our functions and main code sandwiched between template code) *)
    let all_x86 = Buffer.create 1024 in
    let add_fragment frag_file = List.iter (fun line -> Buffer.add_string all_x86 (line ^ "\n")) (read_all frag_file) in
    add_fragment file_frag_1;
    Buffer.add_buffer all_x86 code_funcs;
    add_fragment file_frag_2;
    Buffer.add_buffer all_x86 code;
    add_fragment file_frag_3;

    (* Write the x86 code to the output filepath *)
    let out_channel = open_out fileout in
  	Buffer.output_buffer out_channel all_x86;
	with Failure str -> print_endline str;
