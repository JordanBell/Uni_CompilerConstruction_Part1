type opcode =
  | Plus | Minus | Times | Divide
  | Leq | Geq | Equal | Noteq
  | And | Or

type opcode_unary = Not

type expression =
	| Const of int (* 7 *)
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
	| Let of string * expression * expression (* let x = e in e *)
	| New of string * expression * expression (* new x = e in e *)
	| Deref of expression (* ~e *)
	| Ref of expression (* &e *)
	| Scope of expression (* { e } *)
	| Empty

type fundef = Myfunc of string * string list * expression

type program = fundef list

(* Evaluation *)
type eval_result =
	| Int of int
	| Bool of bool
	| String of string
	| Identifier of string (* Note that this is different from the expression type, 'Identifier'. This will allow an identifier to have the value of another identifier (ie int y = 0; int& x = y) *)
	| Unit

type identifier_definition =
  | Const of eval_result
  | Var of int

type tstore =
{
  decl_funcs : program ref;
  decl_ids   : (string * identifier_definition) list ref;
  tbl_refs  : (int, eval_result) Hashtbl.t ref; 						(* int is the address, eval_result is the value *)
}
