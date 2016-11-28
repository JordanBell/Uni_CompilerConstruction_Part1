type opcode =
  | Plus | Minus | Times | Divide
  | Leq | Geq | Equal | Noteq
  | And | Or

type opcode_unary = Not

type expression =
	| Const_int of int (* 7 *)
	| Const_bool of bool (* true *)
  | Const_string of string (* "hello world" *)
  | Const_struct of string (* MyStruct *) (* Used in: let str_obj = MyStruct in *) (* This will initialise str_obj to default values of MyStruct *)
	| Operator of opcode * expression * expression (* e + e *)
	| Operator_unary of opcode_unary * expression (* !e *)
	| If of expression * expression * expression (* if e { e } else { e } *)
	| Seq of expression * expression (* e; e *)
	| Asg of expression * expression (* e = e *)
	| While of expression * expression (* while e do e *)
	| Readint (* read_int () *)
	| Printint of expression (* print_int (e) *)
	| Application of expression * expression list (* e(e) *)
	| Identifier of string (* x *)
  | Memaccess of expression * expression (* obj.x *)
	| Let of string * expression * expression (* let x = e in e *)
	| New of string * expression * expression (* new x = e in e *)
	| Deref of expression (* ~e *)
	| Ref of expression (* &e *)
	| Scope of expression (* { e } *)
	| Empty

type memlist = (string * expression) list

type structdef = string * memlist (* Example: *) (* x = 0; y = true; z = "error" *)

type fundef = Myfunc of string * string list * expression

type program = fundef list * structdef list (* Consists of functions and struct definitions *)

(* Evaluation *)
type eval_result =
	| Int of int
	| Bool of bool
  | String of string
	| Struct_data of struct_data
	| Identifier of string (* Note that this is different from the expression type, 'Identifier'. This will allow an identifier to have the value of another identifier (ie int y = 0; int& x = y) *)
	| Unit
and struct_data = (string, eval_result) Hashtbl.t (* (ID, value) Hashtbl *)

type identifier_definition =
  | Const of eval_result
  | Var of int


type tstore =
{
  decl_funcs : fundef list ref;
  decl_structs : structdef list ref;
  decl_ids   : (string * identifier_definition) list ref;
  tbl_refs  : (int, eval_result) Hashtbl.t ref; 						(* int is the address, eval_result is the value *)
}

(*    GENERAL FUNCTIONS     *)
let rec iterate f x n = if (n > 0) then iterate f (f x) (n-1) else x (* Apply a function to an argument a given number of times. *)

let rec get_func_def funcdefs with_name = match funcdefs with
	| h::tl ->
		(match h with (Myfunc (func_name, _, _)) ->
		if func_name = with_name
		then h
		else get_func_def tl with_name)
	| [] -> raise (Failure ("Function \"" ^ with_name ^ "\" undeclared"))

let rec get_func_exp program with_name = match program with
	| (Myfunc (func_name, _, e))::tl ->
		if func_name = with_name then e
		else get_func_exp tl with_name
	| [] -> raise (Failure ("Function \"" ^ with_name ^ "\" undeclared"))

let optimisation_iterations = 10
