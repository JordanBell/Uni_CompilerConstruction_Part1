open Parser_types
open Parser_printer

let string_of_eval_result er = match er with
	| Int (i) -> string_of_int i
	| Bool (b) -> if b then "true" else "false"
	| String (s) -> s
	| Identifier (s) -> "ID:" ^ s
	| Unit -> "Unit"

let fun_of_opcode_int = function
	| Plus -> (fun x y -> x + y)
	| Minus -> (fun x y -> x - y)
	| Times -> (fun x y -> x * y)
	| Divide -> (fun x y -> x / y)
	| _ -> failwith "Mismatch: Cannot use operator on int types"

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

let lookup_identifier store id_str = 
	try Hashtbl.find store id_str
	with Not_found -> failwith ("Identifier \'" ^ id_str ^ "\' not declared.")

let rec bool_of_eval_result store e_eval_result =		
	match e_eval_result with
		| Bool (b_value) -> b_value
		| Identifier (str) ->
			let lookup_result = lookup_identifier store str in
			bool_of_eval_result store lookup_result (* Recurse with the new identifier *)
		| _ -> print_eval_result store e_eval_result; failwith "Cannot resolve expression result to bool"

let rec evaluate_expression store i_e = match i_e with
	| Const (i) -> Int (i)
	| Operator (opcode, e, f) -> evaluate_operator store opcode e f
	| Operator_unary (opcode, e) -> evaluate_operator_unary store opcode e
	| If (b, e, f) -> 
		let b_value = (bool_of_eval_result store (evaluate_expression store b)) in
		if b_value  
			then evaluate_expression store e
			else evaluate_expression store f
	| Seq (e, f) -> 
		(* Evaluate both expressions (first expr may alter the store), but only return evaluation of f *)
		let _ = evaluate_expression store e in
		evaluate_expression store f 
	| Asg (Identifier (str), f) -> 
		if (Hashtbl.mem store str)
		then 
			let f_eval_result = evaluate_expression store f in
			Hashtbl.replace store str f_eval_result;
			Identifier (str) (* An assignment returns a reference to the identifier that was newly assigned *)
		else		
			failwith ("Cannot assign a value to identifier \'" ^ str ^ "\' as it has not been declared")

	| Asg (e, f) -> 
		let e_eval_result = evaluate_expression store e in
		(match e_eval_result with
			| Identifier (str) -> evaluate_expression store (Asg (Identifier (str), f))
			| _ -> failwith "Cannot assign a non-identifier expression a value");
	| While (e, f) -> 
		(* Evaluate the condition, repeat if true *)
		let e_eval_result = evaluate_expression store e in
		if (bool_of_eval_result store e_eval_result)
		then 
			let _ = evaluate_expression store f in 	(* Evalute f, as this may modify the store*)
			evaluate_expression store i_e		(* Recurse with the same expression *)
		else Unit

	| Printint (e) -> evaluate_expression store e 		(* Return the value of e. Will do more with this later *)
	| Identifier (str) -> lookup_identifier store str
	| New (str, e, f) -> 
		if (Hashtbl.mem store str)
		then 
			failwith ("Cannot reinstantiate. Variable already exists with identifier: " ^ str)
		else		
			let e_eval_result = evaluate_expression store e in
			Hashtbl.add store str e_eval_result; (* Add a new eval_result for the str identifier *) 
			evaluate_expression store f (* Recurse to the next expression *)

	| Deref (Identifier (str)) -> lookup_identifier store str
	| Deref _ -> failwith "Invalid expression type upon dereference"

	| Ref (Identifier (str)) -> Identifier (str) (* Return an eval_result equivalent to a reference to the identifier *)
	| Ref _ -> failwith "Can only create a reference to an identifier expression"

	| Scope (e) -> evaluate_expression store e (* Recurse into the given scope. Will take scope into account later *)
	| Empty -> Unit
	| Application (e, f) -> failwith "Expression type not implemented: Application"
	| Readint -> failwith "Expression type not implemented: Readint"

and evaluate_operator store opcode e f = 
	(* Get the binary operator for the opcode *)

	(* Evaluate the sub-expressions *)
	let result_e = evaluate_expression store e in 
	let result_f = evaluate_expression store f in
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
		| _, _ -> failwith ("Evaluation mismatch for operator: " ^ (string_of_opcode opcode))

and evaluate_operator_unary store opcode e = 	
	let result_e = evaluate_expression store e in
	match opcode with
	| Not -> (match result_e with
		| Bool (reb) -> Bool (not reb)
		| _ -> failwith "Invalid expression result for unary operator \"Not\"")



