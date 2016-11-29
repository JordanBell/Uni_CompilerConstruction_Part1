open Printf
open Parser
open Parser_lex
open Parser_types

(* Other modules *)
open Parser_printer
open Parser_optimisation
open Parser_evaluator

let filename = Sys.argv.(1)
let num_args = Array.length Sys.argv
let is_verbose = ref false
let is_optimised = ref false
let is_optimisation_comparison = ref false
let store =
{
	decl_funcs = ref [];
	decl_structs = ref [];
  decl_ids   = ref [];
  tbl_refs   = ref (Hashtbl.create 100);
}

let get_arg arg_name =
	if num_args >= 3
	then
		(* Search through the array *)
		Array.fold_left (fun x a -> x || a = arg_name) false Sys.argv
	else false

let () =
	(* Go through optional arguments *)
	is_verbose := get_arg "-verbose";
	is_optimised := get_arg "-o";
	is_optimisation_comparison := get_arg "-oc";

	let parsed_program = parse_file filename !is_verbose in
  (match parsed_program with (fundefs, structdefs) ->
    store.decl_funcs := fundefs;
    store.decl_structs := structdefs);
  let eval_start_exp = ref Empty in

	(* Optimise the program, if the argument was supplied to do so. *)
	(if !is_optimisation_comparison
	then
		let optimised_program = iterate optimise_program !(store.decl_funcs) optimisation_iterations in
		if !is_verbose then print_parse_result optimised_program;

		(* Print out the evalutation times with and without optimisation. *)
		let start_time1 = Sys.time() in
		for i = 0 to 10000 do
  		let _ = eval store (get_func_exp !(store.decl_funcs) "main") in
			()
		done;
		let exe_time1 = Sys.time() -. start_time1 in
		(* Clear the store for future use *)
		store.decl_ids := [];
		store.tbl_refs := Hashtbl.create 100;

  	let start_time2 = Sys.time() in
		for i = 0 to 10000 do
  		let _ = eval store (get_func_exp optimised_program "main") in
			()
		done;
		let exe_time2 = Sys.time() -. start_time2 in
		(* Clear the store for future use *)
		store.decl_ids := [];
		store.tbl_refs := Hashtbl.create 100;

		(* Calculate other stats *)
		let percentage_reduction_time = exe_time2 /. exe_time1 in
		let count_optimised = List.fold_left (fun acc fd -> match fd with Myfunc(_, _, e) -> acc + (count_exp e)) 0 optimised_program in
		let count_default = List.fold_left (fun acc fd -> match fd with Myfunc(_, _, e) -> acc + (count_exp e)) 0 !(store.decl_funcs) in
		let num_pruned_exps = count_default - count_optimised in
		let percentage_reduction = (float_of_int count_optimised) /. (float_of_int count_default) in
		printf "\nOptimisation results:
Execution time of 10,000 evaluations BEFORE optimisation: %fs
Execution time of 10,000 evaluations AFTER optimisation: %fs
Time reduction: %f%% of original
----
Original: %d expressions
Optimised: %d expressions
Pruned %d expressions
Spatial reduction: %f%% of original\n"
					exe_time1
					exe_time2
					(percentage_reduction_time *. 100.)
					count_default
					count_optimised
					num_pruned_exps
					(percentage_reduction *. 100.);
		store.decl_funcs := optimised_program);

	(if !is_optimised
	then
		let optimised_program = iterate optimise_program !(store.decl_funcs) optimisation_iterations in
	  store.decl_funcs := optimised_program);

	eval_start_exp := (get_func_exp !(store.decl_funcs) "main");

	(* Evaluate the program, starting at the main function *)
	try
		let evaluated_expression = eval store !eval_start_exp in
		print_eval_result store evaluated_expression
	with Failure str -> print_endline str;
