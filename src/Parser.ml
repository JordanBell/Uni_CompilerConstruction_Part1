open Printf
open Parser_lex
open Parser_types
open Parser_printer
open Parser_optimisation
open Parser_evaluator

let filename = Sys.argv.(1)
let num_args = Array.length Sys.argv
let optimisation_iterations = 10
let is_verbose = ref false
let is_optimised = ref false
let is_optimisation_comparison = ref false
let store =
{
	decl_funcs = ref [];
  decl_ids   = ref [];
  tbl_refs   = ref (Hashtbl.create 100);
}

let get_arg arg_name =
	if num_args >= 3
	then
		(* Search through the array *)
		Array.fold_left (fun x a -> x || a = arg_name) false Sys.argv
	else false

let rec count_exp e = match e with
	| Operator (op, e, f) -> 1 + count_exp e + count_exp f
	| Operator_unary (op, e) -> 1 + count_exp e
	| If (b, e, Empty) -> 1 + count_exp e
	| If (b, e, f) -> 1 + count_exp e + count_exp f
	| While (b, e) -> 1 + count_exp e
	| Asg (e, f) -> 1 + count_exp e + count_exp f
	| Seq (e, f) -> count_exp e + count_exp f
	| Application (e, fs) -> count_exp e (* TODO This is a bit more complicated. *)
	| Let (str, e, f) -> 1 + count_exp e + count_exp f
	| New (str, e, f) -> 1 + count_exp e + count_exp f
	| Scope (e) -> count_exp e
	| Deref (e) -> 1 + count_exp e
	| Ref (e) -> 1 + count_exp e
	| Printint (e) -> 1 + count_exp e
	| Readint ->  1
	| Const_int (_) | Const_bool (_) | Const_string (_) -> 1
	| Identifier (str) -> 1
	| Empty -> 0

let parse_to_program i_str =
	let i_buf = Lexing.from_string i_str in
	try Parser_par.top Parser_lex.read i_buf
	with
		| SyntaxError str -> printf "Parsing Syntax error: %s\n" str; []
		| Parser_par.Error -> printf "Parser failed\n"; []

let myprint a b = if !is_verbose then printf a b else ()

let () =
	(* Go through optional arguments *)
	is_verbose := get_arg "-verbose";
	is_optimised := get_arg "-o";
	is_optimisation_comparison := get_arg "-oc";

	(* Print the initial message for processing the file *)
	if !is_verbose then
		printf "\n***** Parsing START on file: %s\n" filename
	else
		printf "%s... " filename;

	(* Read in all of the lines *)
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
	myprint "*** Found %d lines\n" num_lines;
	if !is_verbose then printlines !i_lines;
	if num_lines > 0 then
		let lines_as_string = String.concat "\n" !i_lines in
			store.decl_funcs := (parse_to_program lines_as_string);

			if !is_verbose then printf "Result:\n";
			if !is_verbose then print_parse_result !(store.decl_funcs);

			(* Optimise the program, if the argument was supplied to do so. *)
			if !is_optimisation_comparison
			then
				(*iterate (fun x -> printf "%d\n" x; x) 0 optimisation_iterations;*)
				let optimised_program = iterate (List.map optimise_function) !(store.decl_funcs) optimisation_iterations in
				(*let optimised_program = List.map optimise_function !(store.decl_funcs) in*)
				let count_optimised = List.fold_left (fun acc fd -> match fd with Myfunc(_, _, e) -> acc + (count_exp e)) 0 optimised_program in
				let count_default = List.fold_left (fun acc fd -> match fd with Myfunc(_, _, e) -> acc + (count_exp e)) 0 !(store.decl_funcs) in
				let num_pruned_exps = count_default - count_optimised in
				let percentage_reduction = (float_of_int num_pruned_exps) /. (float_of_int count_default) in
				printf "\nOptimisation results:
	Original: %d expressions
	Optimised: %d expressions
	Pruned %d expressions
	Program was reduced by %f%%.\n"
							count_default
							count_optimised
							num_pruned_exps
							(percentage_reduction *. 100.);
							if !is_verbose then print_parse_result optimised_program;
				store.decl_funcs := optimised_program;

			if !is_optimised
			then store.decl_funcs := List.map optimise_function !(store.decl_funcs);;

			(* Evaluate the program, starting at the main function *)
			try
				let evaluated_expression = eval store (get_func_exp !(store.decl_funcs) "main") in
				printf "Evaluates to: ";
				print_eval_result store evaluated_expression
			with Failure str -> print_endline str;

	myprint "***** Parsing FINISHED on file: %s\n\n" filename;
