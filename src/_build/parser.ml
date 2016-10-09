open Printf

type opcode =
  | Plus | Minus | Times | Divide
  | Leq | Geq | Equal | Noteq
  | And | Or | Not

type expression =
  | Seq of 		expression * expression (* e; e *)
  | While of 		expression * expression (* while e do e *)
  | If of 		expression * expression * expression (* if e do e else e *)
  | Asg of 		expression * expression (* e := e *)
  | Deref of 		expression (* !e *)
  | Operator of 	opcode * expression * expression (* e + e *)
  | Application of 	expression * expression (* e(e) *)
  | Const of 		int (* 7 *)
  | Readint 		(* read_int () *)
  | Printint of 	expression (* print_int (e) *)
  | Identifier of 	string (* x *)
  | Let of 		string * expression * expression (* let x = e in e *)
  | New of 		string * expression * expression (* new x = e in e *)

type fundef = string * string list * expression

type program = fundef list

let () = printf "Running on file %s\n" Sys.argv.(1)

(*

Test Program:

pow(int x, int p)
{
	while(p > 1)
	{
		x = x * x;
		p = p - 1;
	}

	return x;
}

main()
{
	print(pow(5, 2));
	print(pow(3, 4));
	print(pow(10, 5));
}

Parsed:


*)
