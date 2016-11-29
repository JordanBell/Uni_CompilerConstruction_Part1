open Parser_types
open Parser_printer
open Printf

let op_counter = ref 0

let addr_counter = ref 0
let new_ref() = addr_counter := !addr_counter + 1; !addr_counter

(* List.assoc wrapped within an error check *)
let lookup_definition str store =
	try List.assoc str !(store.decl_ids)
	with Not_found -> failwith ("Access of an undeclared identifier: " ^ str)

(* Returns eval_resullt *)
let value_of_identifier store str =
	let definition = lookup_definition str store in
	match definition with
		| Const (er)	-> er
		| Var (addr) ->
			try Hashtbl.find !(store.tbl_refs) addr
			with Not_found -> failwith ("Identifier \'" ^ str ^ "\' not declared.")

let fun_of_opcode_int = function
	| Plus -> (fun x y -> x + y)
	| Minus -> (fun x y -> x - y)
	| Times -> (fun x y -> x * y)
	| Divide -> (fun x y -> x / y)
	| _ -> failwith "Mismatch: Cannot use operator on int types"

let fun_of_opcode_string = function
| Plus -> (fun x y -> x ^ y)
| _ -> failwith "Mismatch: Cannot use operator on string types"

let fun_of_opcode_comp = function
	| Geq -> (fun x y -> x >= y)
	| Leq -> (fun x y -> x <= y)
	| Equal -> (fun x y -> x == y)
	| Noteq -> (fun x y -> x != y)
	| _ -> failwith "Mismatch: Cannot use operator on bool types"

let fun_of_opcode_bool = function
	| And -> (fun x y -> x && y)
	| Or -> (fun x y -> x || y)
	| _ -> failwith "Mismatch: Cannot use operator on bool types"

let rec bool_of_eval_result store e_eval_result =
	match e_eval_result with
		| Bool (b_value) -> b_value
		| Int (i_value) -> i_value != 0
		| String (_) | Struct_data (_) -> true
		| Identifier (str) ->
			let lookup_result = value_of_identifier store str in
			bool_of_eval_result store lookup_result (* Recurse with the new identifier *)
		| _ -> print_eval_result store e_eval_result; failwith "Cannot resolve expression result to bool"

(******************************************************************************)

let rec eval store i_e =
	(*printf "\nEvaluating new expression: \n";
	print_expression i_e 0;*)
	op_counter := !op_counter + 1;
	match i_e with

	| Operator (opcode, e, f) -> evaluate_operator store opcode e f

	| Const_int (i) -> Int (i)
	| Const_bool (b) -> Bool (b)
	| Const_string (str) -> String (str)
	| Const_struct (str) ->
		(* Construct new data for that struct type *)
		Struct_data (get_default_construction store str)

	| Operator_unary (opcode, e) -> evaluate_operator_unary store opcode e

	| If (b, e, f) ->
		let b_value = (bool_of_eval_result store (eval store b)) in
		if b_value
			then eval store e
			else eval store f

	| While (e, f) ->
		(* Evaluate the condition, repeat if true *)
		let e_eval_result = eval store e in
		if (bool_of_eval_result store e_eval_result)
		then
			let _ = eval store f in 	(* Evalute f, as this may modify the store *)
			eval store i_e		(* Recurse with the same expression *)
		else Unit

	| Asg (Identifier (str), f) ->
		if List.mem_assoc str !(store.decl_ids)
		then
			let definition = lookup_definition str store in
			match definition with
				| Const _ -> failwith ("Cannot re-assign a new value to a let-bound identifier.")
				| Var (addr) ->
					let f_eval_result = eval store f in
					Hashtbl.replace !(store.tbl_refs) addr f_eval_result;
					Identifier (str) (* An assignment returns a reference to the identifier that was newly assigned *)
		else
			failwith ("Cannot assign a value to identifier \'" ^ str ^ "\' as it has not been declared")
	| Asg (Memaccess (Identifier(id_parent), Identifier(id_child)), f) ->
		if List.mem_assoc id_parent !(store.decl_ids)
		then
			let definition = lookup_definition id_parent store in
			match definition with
				| Const (er) ->
					(match er with
						| Struct_data (s_data) ->
							(* Make sure the child ID is a part of the struct *)
							if not (Hashtbl.mem s_data id_child) then failwith ("Struct-child identifier \'" ^ id_child ^ "\' not within the struct's declaration.");

							(* Set the value of the child to the evaluation result of f *)
							let f_eval_result = eval store f in
							Hashtbl.replace s_data id_child f_eval_result;

							(* An assignment returns an Empty expression for child assignments *)
							Unit
						| _ -> failwith ("Cannot assign child identifiers of a non-struct type " ^ (string_of_eval_result_type er) ^", in the expression \"" ^ id_parent ^ "." ^ id_child ^ "\""))
					(* failwith ("Cannot re-assign a new value to a child in a let-bound struct. Note: It is recommended to ALWAYS create struct variables using the \"new\" keyword, instead of \"let\".") *)
				| Var (addr) ->
					try
						(let struct_value = Hashtbl.find !(store.tbl_refs) addr in

						(match struct_value with
							| Struct_data (s_data) ->
								(* Make sure the child ID is a part of the struct *)
								if not (Hashtbl.mem s_data id_child) then failwith ("Struct-child identifier \'" ^ id_child ^ "\' not within the struct's declaration.");

								(* Set the value of the child to the evaluation result of f *)
								let f_eval_result = eval store f in
								Hashtbl.replace s_data id_child f_eval_result;

								(* An assignment returns an Empty expression for child assignments *)
								Unit
							| _ -> failwith "Cannot access child identifiers of a non-struct type"))
					with Not_found -> failwith ("Struct identifier \'" ^ id_parent ^ "\' not declared.")
		else
			failwith ("Cannot assign a value to struct identifier \'" ^ id_parent ^ "\' as it has not been declared")
	| Asg (Memaccess (_, _), _) -> failwith "Must directly access a struct's data by it and its parent's identifer"
	| Asg (e, f) ->
		let e_eval_result = eval store e in
		(match e_eval_result with
			| Identifier (str) -> eval store (Asg (Identifier (str), f))
			| _ -> failwith "Cannot assign a non-identifier expression a value");

	| Let (str, e, f) ->
		let e_eval_result = eval store e in
		store.decl_ids := ((str, Const (e_eval_result))::!(store.decl_ids));
		let result = eval store f in (* Recurse to the next expression in an updated environment *)
		store.decl_ids := List.tl !(store.decl_ids);
		result

	| New (str, e, f) ->
		let e_eval_result = eval store e in
		let this_ref = new_ref() in
		store.decl_ids := ((str, Var (this_ref))::!(store.decl_ids));
		Hashtbl.add !(store.tbl_refs) this_ref e_eval_result; (* Add a corresponding eval_result to the store for the new reference *)
		let result = eval store f in 	(* Recurse to the next expression in an updated environment, linking the str identifier to its reference number *)
		store.decl_ids := List.tl !(store.decl_ids);
		result

	| Seq (e, f) ->
		(* Evaluate both expressions (first expr may alter the store), but only return evaluation of f *)
		let _ = eval store e in
		eval store f

	| Identifier (str) ->
		let definition = lookup_definition str store in
		(match definition with
		 	| Const (er) -> er
			| Var (_) -> failwith ("Cannot implicitly dereference variable \"" ^ str ^ "\""))

	| Memaccess (_, _) -> eval store (Deref (i_e))

	| Printint (e) ->
		let e_eval_result = eval store e in
		(match e_eval_result with
			| String (msg) -> printf "%s\n" msg
			| Int (i) -> printf "%d\n" i
			| Bool (b) -> printf "%s\n" (if b then "true" else "false")
			| Struct_data _ -> failwith "Cannot print struct data"
			| _ -> failwith "Cannot print that kind of expression");
		e_eval_result

	| Deref (Identifier (str)) ->
	 	let definition = lookup_definition str store in
		(match definition with
			| Const (er) ->
				(* Dereferencing a let-bound identifier is only allowed if that identifier, in itself, is a reference. *)
				(match er with
					| Identifier (ref_str) -> eval store (Deref (Identifier (ref_str)))
					| _ -> failwith ("Cannot dereference a let-bound constant identifier: " ^ str))
			| Var (addr) ->
				try Hashtbl.find !(store.tbl_refs) addr
				with Not_found -> failwith ("Identifier \'" ^ str ^ "\' not declared."))
	| Deref (Memaccess (Identifier (id_parent), Identifier (child_lookup_id))) ->
		let e_result = value_of_identifier store id_parent in
		(match e_result with
			| Struct_data (s_data) ->
				(try Hashtbl.find s_data child_lookup_id (* Get the value assigned to that child variable *)
				with Not_found -> failwith ("Error on dereference: There is no member \"" ^ child_lookup_id ^ "\" in the struct identifier, \"" ^ id_parent ^ "\""))
			| _ -> failwith ("Cannot access the memory of a non-struct identifier of type " ^ (string_of_eval_result_type e_result) ^ " with id, \"" ^ id_parent ^ "\" (When accessing data with id, \"" ^ child_lookup_id ^ "\")"))

	| Deref _ -> failwith "Invalid expression type upon dereference"

	| Ref (Identifier (str)) -> Identifier (str) (* Return an eval_result equivalent to a reference to the identifier *)
	| Ref _ -> failwith "Can only create a reference to an identifier"

	| Scope (e) -> eval store e (* Recurse into the given scope. Will take scope into account later *)

	| Application (Identifier(str), arg_values) ->
		(* Get the name of the function we are apply the arguments to. This may be a direct identifier, or a function pointer. If it's a function pointer, extract the name of the function the pointer is pointing to. *)
		let func_name =
		(
			try
				(* Check if the identifier is a reference to a function *)
				let as_ref_eval_result =
					(match lookup_definition str store with
						| Const (er)	-> er
						| Var (addr) -> Hashtbl.find !(store.tbl_refs) addr)
				in
				(
					match as_ref_eval_result with
						| Identifier(ref_id_str) ->
							(
								try
									let _ = get_func_def !(store.decl_funcs) ref_id_str in
									ref_id_str (* If the above function call raised no issues, use the ref_id_str value as the name of the applied function. *)
								with _ -> raise Not_found
							) (* This does point to another value, but that value is not a declared function. *)
						| _ -> raise Not_found (* There exists a variable/const with the same name, but it is not an identifier pointer. *)
				)
			with _ -> str
		)
		in
		(* Find the program with the identifier name *)
		(match get_func_def !(store.decl_funcs) func_name with Myfunc (_, arg_names, func_exp) ->
			(* Check if the arg list matches in size *)
			if (List.length arg_names) != (List.length arg_values) then failwith ("Argument count mismatch for function \"" ^ func_name ^ "\": Was expecting " ^ string_of_int (List.length arg_names) ^ " arguments but received " ^ string_of_int (List.length arg_values));

			(* Recurse with an expression created from matching the values to the names *)
			let func_store =
			{
				decl_funcs = store.decl_funcs;
				decl_structs = store.decl_structs;
			  decl_ids   = ref [];
			  tbl_refs   = ref (Hashtbl.create 100);
			}
			in
			build_function_store store (ref func_store) arg_names arg_values;
			eval func_store func_exp) (* Evaluate the function with its own store, containing the provided argument variables *)
	| Application (_, _) -> failwith "Application begin with an identifier"

	| Readint -> failwith "Expression type not implemented: Readint"

	| Empty -> Unit

and evaluate_operator store opcode e f =
	(* Get the binary operator for the opcode *)

	(* Evaluate the sub-expressions *)
	let result_e = eval store e in
	let result_f = eval store f in
	match result_e, result_f with
		(* Match the eval_results with appropriate opcodes *)
		| Int (rei), Int (rfi) ->
			(match opcode with
				| Plus | Minus | Times | Divide ->
					let int_result = (fun_of_opcode_int opcode) rei rfi in
					Int (int_result)
				| Geq | Leq | Equal | Noteq ->
					let bool_result = (fun_of_opcode_comp opcode) rei rfi in
					Bool (bool_result)
				| _ -> failwith "Cannot perform non-int operations on int values")
		| Bool (rei), Bool (rfi) ->
			(let opcode_as_fun = fun_of_opcode_bool opcode in
			match opcode with
				| And | Or ->
					let bool_result = opcode_as_fun rei rfi in
					Bool (bool_result)
				| _ -> failwith "Cannot perform non-bool operations on bool values")
		| String (res), String (rfs) ->
			(let opcode_as_fun = fun_of_opcode_string opcode in
			match opcode with
				| Plus ->
					let string_result = opcode_as_fun res rfs in
					String (string_result)
				| _ -> failwith "Cannot perform non-bool operations on bool values")
		| _, _ ->
			(match opcode with
				| Noteq -> Bool (true) (* If there is a type mismatch, they are therefore not equal *)
				| _ 		-> Bool (false)) (* By default, just return false for invalid operation *)

and evaluate_operator_unary store opcode e =
	let result_e = eval store e in
	match opcode with
	| Not -> (match result_e with
		| Bool (reb) -> Bool (not reb)
		| _ -> failwith "Invalid expression result for unary operator \"Not\"")

and build_function_store store func_store arg_names arg_values = match arg_names, arg_values with
| n::ns, e::es ->
	(* Add a constant identifier for the parameter/argument matchup *)
	let e_eval_result = eval store e in
	(!func_store).decl_ids := ((n, Const (e_eval_result))::!((!func_store).decl_ids));
	(* Recurse through the rest of the parameters/arguments *)
	build_function_store store func_store ns es
| [], [] -> ()
| _ -> failwith "Mismatch between arg_names and arg_values"

and get_default_construction store struct_type_id =
	(* Lookup the struct_type_id's declaration in the store *)
	let object_data = Hashtbl.create 8 in (* This will be filled with data for the struct object *)
	let member_list = List.assoc struct_type_id !(store.decl_structs) in
	(* For each member of the struct definition, create an initialised variable to be put into the struct object *)
	let init_data memdef = match memdef with (mem_id, initial_e) ->
		(* Determine the initial value *)
		let initial_value = eval store initial_e in
		Hashtbl.add object_data mem_id initial_value
	in
	List.iter init_data member_list;
	object_data
