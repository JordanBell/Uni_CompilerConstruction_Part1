type opcode =
  | Plus | Minus | Times | Divide
  | Leq | Geq | Equal | Noteq
  | And | Or

type opcode_unary = Not

type expression =
	| Const of int (* 7 *)
	| Operator of opcode * expression * expression (* e + e *)
	| Operator_unary of opcode_unary * expression (* NOT e *)
	| If of expression * expression * expression (* if e do e else e *)
	| Seq of expression * expression (* e; e *)
	| Asg of expression * expression (* e := e *)
	| While of expression * expression (* while e do e *)
	| Readint (* read_int () *)
	| Printint of expression (* print_int (e) *)
	| Application of expression * expression (* e(e) *)
	| Identifier of string (* x *)
	| New of string * expression * expression (* new x = e in e *)
	| Deref of expression (* !e *)
	| Scope of expression (* { e } *)
	| Empty

type fundef = Myfunc of string * string list * expression

type program = fundef list
