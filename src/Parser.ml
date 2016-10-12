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

let filename = Sys.argv.(1)

let rec printlines = function
	| [] -> printf ""
	| h::tl -> printf "%s\n" h; printlines tl

let do_parse i_str = 
	let i_buf = Lexing.from_string i_str in
	
	

let () = 
	printf "\n***** Parsing START on file: %s\n" filename;
	let i_lines = ref [] in
	let channel = open_in filename in
	try
	  while true; do
	    let this_line = input_line channel in
	    i_lines := this_line :: !i_lines
	  done;
	with End_of_file ->
	  close_in channel;
	  i_lines := List.rev !i_lines;
	let num_lines = List.length !i_lines in
	printf "*** Found %d lines\n" num_lines;
	printlines !i_lines;
	if num_lines > 0 then
		let lines_as_string = String.concat "\n" !i_lines in
		do_parse lines_as_string;;
	printf "***** Parsing END on file: %s\n\n" filename




	(*let opened_file = open_in filepath in opened_file
  	try 
	    let line = input_line opened_file in	
	    print_endline line;          (* write the result to stdout *)
	    flush stdout;                (* write on the underlying device now *)
	    close_in ic                  (* close the input channel *) 
	with e -> close_in_noerr ic; raise e*)





(**)


	(*printf "Running on file %s\n" filepath*)

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
