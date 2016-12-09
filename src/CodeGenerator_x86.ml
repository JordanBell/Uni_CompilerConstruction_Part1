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

let decr_sp () =
  "\t# Sp-- to " ^ (string_of_int (!sp - 1)) ^ " (%" ^ (string_of_int (!stack_offset - (8 * (!sp - 1)))) ^ ")\n" |> Buffer.add_string code;
  sp := !sp - 1

let incr_sp () =
  "\t# Sp++ to " ^ (string_of_int (!sp + 1)) ^ " (%" ^ (string_of_int (!stack_offset - (8 * (!sp + 1)))) ^ ")\n" |> Buffer.add_string code;
  sp := !sp + 1


let string_of_opcode_x86 opc = match opc with
  | Plus | Or -> "add" (* Or also uses the add instruction, as two Or'd values will be non-zero if either are non-zero. A known issue is that this will cause unexpected results when Or-ing two integers such as -7 and 7 (which will equal "false"), but this is acceptable as implicit int-to-bool casting is ill-advised *)
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
  | And ->
    let s_lbl_false = ".BOP" ^ (string_of_int !bool_op_count) in
    let s_lbl_continue = ".CONT_BOP" ^ (string_of_int !bool_op_count) in
    "\tpop\t%rax\n" ^
    "\tpop\t%rbx\n" ^
    "\tcmp\t$0, %rax\n" ^
    "\tje\t" ^ s_lbl_false ^ "\n" ^
    "\tcmp\t$0, %rbx\n" ^
    "\tje\t" ^ s_lbl_false ^ "\n" ^
    "\tpush\t%rax\n"
     |> Buffer.add_string code;

    (* If TRUE *)
    "\tpush\t$1\n" ^ (* Push a value of TRUE *)
    "\tjmp\t" ^ s_lbl_continue ^ "\n" |> Buffer.add_string code;

    (* If FALSE *)
    s_lbl_false ^ ":\n" ^
    "\tpush\t$0\n" |> Buffer.add_string code; (* Push a value of TRUE *)

    (* Continue *)
    s_lbl_continue ^ ":\n" |> Buffer.add_string code;

    (* Increment Bool-op count *)
    bool_op_count := !bool_op_count + 1
  | _ ->
  "\tpop\t%rax\n" ^
  "\tpop\t%rbx\n" ^
  "\t" ^ (string_of_opcode_x86 opc) ^ "\t%rax, %rbx\n" ^
  "\tpush\t%rbx\n" |> Buffer.add_string code

let codegenx86_not () =
  let s_lbl_make_true = ".BOP" ^ (string_of_int !bool_op_count) in
  let s_lbl_continue = ".CONT_BOP" ^ (string_of_int !bool_op_count) in
  "\tpop\t%rax\n" ^
  "\tcmp\t$0, %rax\n" ^
  "\tje\t" ^ s_lbl_make_true ^ "\n" ^

  (* If top of stack = "true" *)
  "\tpush\t$0\n" (* Push "false" *)
   |> Buffer.add_string code;

  (* If top of stack = "false" *)
  s_lbl_make_true ^ ":\n" ^ (* Push "true" *)
  "\tpush\t$1\n" |> Buffer.add_string code; (* Push a value of TRUE *)

  (* Continue *)
  s_lbl_continue ^ ":\n" |> Buffer.add_string code;

  (* Increment Bool-op count for new labels in the future *)
  bool_op_count := !bool_op_count + 1

let codegenx86_let () =
  "# Let \n" ^
  "\tpop\t%rax\n" ^
  "\tpop\t%rbx\n" ^
  "\tpush\t%rax\n"
  |> Buffer.add_string code

let codegenx86_id addr id_str =
  "\t# offset " ^ (string_of_int addr) ^ "\n" ^
  "\tmov\t" ^ (!stack_offset - (8 * addr) |> string_of_int) ^ "(%rbp), %rax\t# " ^ id_str ^ "\n" ^
  "\tpush\t%rax\n"
  |> Buffer.add_string code

let codegenx86_st n =
  "\tpush\t$" ^ (string_of_int n) ^ "\n" |> Buffer.add_string code

let sizeof_struct structdefs id_struct =
  let struct_mems = List.assoc id_struct structdefs in
  List.length struct_mems

let get_member_index structdefs id_struct id_member_search =
  let struct_mems = List.assoc id_struct structdefs in
  let rec index_of x = function
    | [] -> -1
    | (id_member, _)::tl ->
      if id_member = x
      then 0
      else 1 + index_of x tl
  in
  index_of id_member_search struct_mems

let rec codegenx86 symt structdefs e_in =
  let frec = codegenx86 symt structdefs in
  match e_in with
  | Operator_unary (opcode, e) ->
      (match opcode with Not ->
      frec e;
      codegenx86_not ())

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
        decr_sp ()
      | _ ->
        frec e1;
        frec e2;
        codegenx86_op op;
        decr_sp ())

  | Identifier (id_str) | Deref (Identifier (id_str)) ->
    let addr = List.assoc id_str symt in
    codegenx86_id addr id_str;
    incr_sp ()

  | Memaccess (_, _) -> failwith "Expression not implemented: Memaccess"

  | Let (x, e1, e2) | New (x, e1, e2) ->
    "\t# " ^ x ^ " declared as: \n" |> Buffer.add_string code;
    frec e1;
    codegenx86 ((x, !sp) :: symt) structdefs e2;
    codegenx86_let ();
    decr_sp ()

  | Ref (Identifier(id_str)) ->
    let addr = List.assoc id_str symt in
    let addr_reg = (!stack_offset - (8 * addr) |> string_of_int) ^ "(%rbp)" in
    "\t# ref to offset " ^ (string_of_int addr) ^ "\n" ^
    "\tleaq\t" ^ addr_reg ^ ", %rax\n" ^
    "\tpush\t%rax\n"
    |> Buffer.add_string code

  | Ref (_) -> failwith "Expression not implemented: Ref (of non-identifier expression)"

  | Asg (Identifier (id_str), e) ->
    frec e;
    let addr = List.assoc id_str symt in
    let str_var_reg = (!stack_offset - 8 * addr |> string_of_int) ^ "(%rbp)" in
    "\t# write (" ^ id_str ^ "), offset " ^ (string_of_int addr) ^ "\n" ^
    "\tpop\t%rax\n" ^
    "\tmov\t%rax, " ^ str_var_reg ^ "\n" ^
    "\tpush\t" ^ str_var_reg ^ "\n"
    |> Buffer.add_string code

  | Seq (e, f) ->
    let _ = frec e in
    "# (Pop previous value in sequence)\n" ^
    "\tpop\t%rax\n"
    |> Buffer.add_string code;
    decr_sp ();
    frec f

  | Const_int n ->
    codegenx86_st n;
    incr_sp ()

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
    decr_sp ();

    (* Increase the counter of if statement labels *)
    ifelse_count := !ifelse_count + 1;

    (* If not jumping, just execute the else code (where the condition equals 0, ie FALSE) *)
    "### If false: \n" |> Buffer.add_string code;
    frec e_iffalse;
    ("\tjmp\t" ^ str_label_continue ^ "\n") |> Buffer.add_string code;


    "### If true: \n" |> Buffer.add_string code;
    sp := !sp - 1; (* Reset sp after the increase from generating if-false code *)
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
    decr_sp ();

    (* Generate code for the loop expression *)
    frec f;

    (* Discard the value pushed by f onto the stack *)
    "\tpop\t%rax\n" |> Buffer.add_string code;
    decr_sp ();

    (* Jump to the start of the loop, where we recalculate the condition *)
    "\tjmp\t" ^ str_label_loop ^ "\n" |> Buffer.add_string code;
    str_label_continue   ^ ":\n" ^
    "\t# Push back the final value of the while loop to give it a value\n" ^
    "\tpush\t%rax\n"|> Buffer.add_string code


  | Application (Identifier (str_func), args) ->
    (* Push all of the args onto the stack *)
    let rec frec_all = function
      | h::tl -> frec h; frec_all tl
      | _ -> ()
    in
    frec_all args;

    let num_args = List.length args in
    let str_num_args = string_of_int num_args in
    "\tcall\t" ^ str_func ^ "\n" ^
    "### Discard the arguments that were pushed onto the stack. Num arguments discarded: " ^ str_num_args ^ "\n"
    |> Buffer.add_string code;

    (*"\tsub\t$" ^ str_num_args ^ ", %rsp\n" ^ (* Discard the arguments into the redzone *)*)

    let rec pop_all = function
      | h::tl -> "\tpop\t%rbx\n" |> Buffer.add_string code; pop_all tl
      | _ -> ()
    in
    pop_all args;


    "\tpush\t%rax\n" ^
    "### Stack Pointer: " ^ (string_of_int (!sp + (1 - num_args))) ^ "\n"
    |> Buffer.add_string code;
    sp := !sp + (1 - num_args) (* Increment once for the pushed return value, decrement per popped argument. *)

  | Printint (e) ->
    (*failwith "Expression not implemented: print"*)
    frec e;
    "### Printing the top of the stack (int):\n" ^
    "\tmovl\t(%rsp), %eax\n" ^
    "\tmovl\t%eax, %esi\n" ^
    "\tmovl\t$.LC0, %edi\n" ^
    "\tmovl\t$0, %eax\n" ^
    "\tcall\tprintf\n"
    |> Buffer.add_string code(*;
    decr_sp ()*)

  | Empty -> ()
  | Const_string (str) -> failwith "Const_strings not implemented for running with instruction sets."
  | Const_struct (id_struct) -> failwith "Expression not implemented: Const_struct"
  | Application (_, _) -> failwith "Expression not implemented: Assignment (with non-simple identifier expression as the function)"
  | Asg (_, _) -> failwith "Expression not implemented: Asg (with a non-simple identifier on the left)"
  | Deref _ ->	failwith "Expression not implemented: Deref (applied to a non-simple identifier)"
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

let generate_function structdefs f = match f with Myfunc (str_func, args, e) ->
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
      sp := !sp + 1; (* For the pushed base pointer *)
      codegenx86 (!symt) structdefs e; (* Generate function code *)
      stack_offset := stack_offset_default;
      sp := 0;

      (* Footer stack pointer and return operations *)
      "\tpopq\t%rax\n" ^
      "\tmovq %rbp, %rsp\n" ^ (* Move the stack pointer to the function-relative base position; leave the local variables in the red zone *)
      "\tpopq\t%rbp\n" ^
      "\tret\n"
      |> Buffer.add_string code
    )

let () =
	(* Go through optional arguments *)
	is_verbose := (get_arg "-verbose") || (get_arg "-v");

	let parsed_program = (parse_file filein !is_verbose) in

  try
  match parsed_program with (funcdefs, structdefs) ->

    let file_frag_dir = "test_cases/CodeGeneration/GeneratedCode/x86/" in
    let file_frag_1 = file_frag_dir ^ "1.x86frag" in
    (* x86 function code will be concatenated here *)
    let file_frag_2 = file_frag_dir ^ "3.x86frag" in
    (* main function code will be concatenated here *)
    let file_frag_3 = file_frag_dir ^ "5.x86frag" in

    Buffer.clear code; (* Clear the buffer *)
    (* Add function code for each function definition in the parsed program *)
    List.iter (generate_function structdefs) funcdefs;

    (* Copy over the generated code to a new function, so the general purpose code buffer can be reused. *)
    let code_funcs = Buffer.create 100 in
    Buffer.add_bytes code_funcs (Buffer.to_bytes code);
    Buffer.clear code; (* Clear the buffer to be re-used for the main code *)

    (* Perform main Code generation *)
    let eval_start_exp = get_func_exp funcdefs "main" in
    Buffer.add_string code "\n\n\t# Generated code START\n";
    let _ = codegenx86 [] structdefs eval_start_exp in
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
