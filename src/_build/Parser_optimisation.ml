open Parser_types
open Parser_evaluator
open Parser_printer
open Printf

let dummy_store =
{
	decl_funcs = ref [];
	decl_structs = ref [];
  decl_ids   = ref [];
  tbl_refs   = ref (Hashtbl.create 1);
}

let rec fold_exp f acc exp =
  match exp with
  (* 3 Sub expressions *)
  | If (b, e, e')
    -> fold_exp f (fold_exp f (fold_exp f (f acc exp) b) e) e'
  (* 2 Sub expressions *)
  | Operator (_, e, e') | While (e, e') | Asg (e, e') | Seq (e, e') | Let (_, e, e') | New (_, e, e')
    -> fold_exp f (fold_exp f (f acc exp) e) e'
  (* 1 Sub Expression *)
  | Scope (e) | Deref (e) | Ref (e) | Operator_unary (_, e) | Printint (e)
    -> fold_exp f (f acc exp) e
  (* 0 Sub Expressions *)
  | Const_int _ | Const_bool _ | Const_string _ | Const_struct _ | Readint | Identifier _ | Memaccess (_, _) | Empty
    -> (f acc exp)
  (* Flexible-length Sub Expressions *)
  | Application (e, fs)
    -> List.fold_left (fun acc sub_e -> fold_exp f acc sub_e) (f acc e) fs

let apply_to_subexpressions p e = match e with
  | Operator (op, e, f) -> Operator(op, p e, p f)
  | Operator_unary (op, e) -> Operator_unary(op, p e)
  | If (b, e, Empty) -> If (p b, p e, Empty)
  | If (b, e, f) -> If (p b, p e, p f)
  | While (b, e) -> While(p b, p e)
  | Asg (e, f) -> Asg (p e, p f)
  | Seq (e, f) -> Seq (p e, p f)
  | Application (e, fs) -> Application (p e, List.map p fs) (* TODO This is a bit more complicated. *)
  | Let (str, e, f) -> Let (str, p e, p f)
  | New (str, e, f) -> New (str, p e, p f)
  | Scope (e) -> Scope (p e)
  | Deref (e) -> Deref (p e)
  | Ref (e) -> Ref (p e)
  | Printint (e) -> Printint (p e)
  | Readint -> e
  | Const_int _ | Const_bool _ | Const_string _ | Const_struct _ -> e
  | Identifier _ -> e
  | Empty -> e
	| Memaccess (e, f) -> Memaccess (p e, p f)


let contains_id_string search_str acc = function
  | Empty -> acc
  | Identifier (str) -> acc || str = search_str
  | _ -> acc

let get_id_strings acc e = match e with
  | Empty -> acc
  | Identifier (str) -> str::acc
  | _ -> acc

let is_id_constant store id_str =
  (* Look up the id's definition in the store *)
  let id_def = lookup_definition id_str store in (* TODO Replace this will the lookup function in the store *)
  (match id_def with
    | Const (_) -> true
    | Var (_) -> false)

let rec foldf_is_modified_in search_str acc = function
	| Asg (e, f) -> acc || (contains_id_string search_str false e)
	| _ -> acc

let is_modified_in id_str = fold_exp (foldf_is_modified_in id_str) false

let rec do_until_modified id_str f es = match es with
| h::tl -> if(is_modified_in id_str h) then (f h)::tl else (f h)::(do_until_modified id_str f tl)
| [] -> []

let rec replace_exp_until_modification_of id_str e_newval e_in =
	let frec = replace_exp_until_modification_of id_str e_newval in (* The partial function we're recursing with *)
	match e_in with
	| Operator (op, e, f) ->
		if (is_modified_in id_str e)
		then e_in
		else if (is_modified_in id_str f)
		then Operator (op, frec e, f)
		else Operator (op, frec e, frec f)
	| Operator_unary (op, e) -> Operator_unary(op, frec e)
	| If (b, e, Empty) ->
		if (is_modified_in id_str b)
		then If (frec b, e, Empty)
		else If (frec b, frec e, Empty)
	| If (b, e, f) ->
	 	if (is_modified_in id_str b)
		then If (frec b, e, f)
		else If (frec b, frec e, frec f)

	| While (b, e) ->
		if (is_modified_in id_str b)
		then While (frec b, e)
		else While (frec b, frec e)
	| Asg (Identifier (id_str'), f) -> if (id_str = id_str') then e_in else Asg (Identifier (id_str'), frec f) (* Stop recursion if the identifier matches the argument *)
	| Asg (e, f) ->
		if (is_modified_in id_str e)
		then Asg (frec e, f)
		else Asg (frec e, frec f)
	| Seq (e, f) ->
		if (is_modified_in id_str e)
		then Seq (frec e, f)
		else Seq (frec e, frec f)
	| Let (str, e, f) ->
		if (str = id_str)
		then e_in  (* If this is an over-shadowing identifier, stop recursion. *)
		else if (is_modified_in id_str e)
		then Let (str, frec e, f)
		else Let (str, frec e, frec f)
	| New (str, e, f) ->
		if (str = id_str)
		then e_in  (* If this is an over-shadowing identifier, stop recursion. *)
		else if (is_modified_in id_str e)
		then New (str, frec e, f)
		else New (str, frec e, frec f)
	| Deref (Identifier (deref_str)) -> if (deref_str = id_str) then e_newval else Deref (Identifier (deref_str)) (* Do the replacing! *)
	| Deref (e) -> Deref (frec e)
	| Scope (e) -> Scope (frec e)
	| Ref (e) -> Ref (e) (* Stop recursing after a reference has been made, for safe measure. *)
	| Printint (e) -> Printint (frec e)

	(* 0 Sub Expressions *)
	| Const_int _ | Const_bool _ | Const_string _ | Const_struct _ | Readint | Identifier _ | Empty
		-> e_in

	(* Flexible-length Sub Expressions *)
	| Application (e, fs) ->
    Application(e, do_until_modified id_str frec fs)

	| Memaccess (e, f) -> Memaccess (frec e, frec f)

let rec replace_constant_with_value id_str e_val e_in = match e_in with
	| Empty -> e_in
	| Identifier (id_str') -> if (id_str = id_str') then e_val else e_in
	| _ -> apply_to_subexpressions (replace_constant_with_value id_str e_val) e_in

let rec exp_with_inline_args e_in arg_names arg_values = match arg_names, arg_values with
  | n::ns, e::es ->
    let after_replacement = replace_constant_with_value n e e_in in
    exp_with_inline_args after_replacement ns es
  | [], [] -> e_in
  | _ -> failwith "Mismatch between arg_names and arg_values"

let rec optimise_exp program_context e =
  let frec = optimise_exp program_context in (* "frec" = "f"unction "rec"ursion *)
	match e with
	| Operator(opcode, Const_int i, Const_int j) ->
    let reduced_evaluation = evaluate_operator dummy_store opcode (Const_int (i)) (Const_int (j)) in
    (match reduced_evaluation with
      | Int (ei) -> Const_int (ei)
      | Bool (eb) -> Const_bool (eb)
      | _ -> failwith "An integer operation somehow returned a non-integer and non-boolean result.")
  | Operator(opcode, Const_bool b1, Const_bool b2) ->
    let reduced_evaluation = evaluate_operator dummy_store opcode (Const_bool (b1)) (Const_bool (b2)) in
    (match reduced_evaluation with
      | Bool (eb) -> Const_bool (eb)
      | _ -> failwith "A boolean operation somehow returned a non-boolean result.")

  | Operator_unary(opcode, Const_bool b) ->
    let reduced_evaluation = evaluate_operator_unary dummy_store opcode (Const_bool (b)) in
    (match reduced_evaluation with
      | Bool (eb) -> Const_bool (eb)
      | _ -> failwith "A boolean operation somehow returned a non-boolean result.")

	| Let (id_str, e_def, e_in) ->
		(* Replace all occurrances of the id in the e_in expression IFF its definition is a constant value *)
		(match e_def with
			| Const_int _ | Const_bool _ | Const_string _ ->
				let replaced_in = replace_constant_with_value id_str e_def e_in in
				frec replaced_in
			| _ -> Let (id_str, frec e_def, frec e_in))

	| New (id_str, e_def, e_in) ->
		(* Replace instances of the variable up until its first assignment to a new value, or declaration of an over-shadowing identifier. *)
		(match e_def with
			| Const_int _ | Const_bool _ | Const_string _ ->
				let replaced_in = replace_exp_until_modification_of id_str e_def e_in in
				New (id_str, e_def, frec replaced_in)
			| _ -> New (id_str, frec e_def, frec e_in)) (* Do nothing if the definition is non-const, as its evaluation may modify the store, etc. *)

	| If(Const_bool b, e, f) -> if b then frec e else frec f
  | If(Const_int i, e, f) -> if (i != 0) then frec e else frec f

  | While(b, e) ->
    (match b with
      | Const_bool (bv) ->
        if bv
        then failwith "Infinite loop detected. Language does not currently support breaking loops with a constant expression."
        else Empty
      | Const_int (iv) ->
        if iv != 0
        then failwith "Infinite loop detected. Language does not currently support breaking loops with a constant expression."
        else Empty
      | _ ->
        let optim_b = iterate frec b 3 in
        let condition_identifiers = fold_exp get_id_strings [] optim_b in
        if List.length condition_identifiers = 0
        then
          (* The condition is too complicated at the moment. Just recurse. *)
          While (optim_b, frec e)
        else
          (* TODO Check if the condition is constant. Only check for the identifier's presence in the main body if the identifier is variable. No need if it is constnat, as we already know it will never change. In that case, immediately throw an error if it evaluates to true. *)
          let e_contains_condition_identifiers = List.fold_left (fun acc id_str -> acc || (fold_exp (contains_id_string id_str) false e)) false condition_identifiers in
          if (not e_contains_condition_identifiers)
          then failwith "Infinite loop detected: The while loop's condition contains identifiers that are not accessed within the loop itself."
          (* TODO If there are none of the identifiers in the body, only throw the error if the condition (which is now considered to be constant) evaluates to true. *)
          else While (optim_b, frec e)) (* Recurse to expression *)

	| Application (Identifier (func_id), fs) ->
    (* Perform constant inlining if each argument is constant *)
    let is_f_const_arg =
      (fun acc arg ->
      (match arg with
        | Const_int _ | Const_bool _ | Const_string _ -> acc && true
        | _ -> false))
    in
    let all_args_constant = (List.fold_left is_f_const_arg true fs) in
    if all_args_constant = false
    then Application(Identifier (func_id), List.map frec fs) (* Do nothing here, just recurse through the argument expressions *)
    else
		(* Find the definition of the function we are calling *)
		(match get_func_def program_context func_id with Myfunc (_, arg_names, func_exp) ->
			(* Check if the arg list matches in size *)
			if (List.length arg_names) != (List.length fs) then failwith ("Argument count mismatch for function \"" ^ func_id ^ "\": Was expecting " ^ string_of_int (List.length arg_names) ^ " arguments but received " ^ string_of_int (List.length fs));
      (* Find what the expression looks like with its parameter identifiers replaced with the const values. *)
      let func_exp_with_replaced_args = exp_with_inline_args func_exp arg_names fs in
      (* Replace this application with the function's optimised expression with constant arguments. *)
      let optimised_func_exp = iterate (optimise_exp program_context) func_exp_with_replaced_args optimisation_iterations in
      optimised_func_exp)


  | Application (e, fs) -> Application (frec e, List.map frec fs)

  | _ -> apply_to_subexpressions frec e

and optimise_function program_context func =
  match func with Myfunc (str, args, e) ->
    Myfunc (str, args, optimise_exp program_context e)

(* Optimise a program, function by function, with an updated program context for each optimisation. Note that the updated context is needed for constant function inlining *)
and optimise_program_with_updating_program os uos = match uos with
	(* Note - os are optimised functions. uos are un-optimised functions *)
	| h::tl ->
    let program_context = os @ uos in (* The optimised program thus far *)
		let h_o = optimise_function program_context h in (* Optimise the head *)
    let os' = (os @ [h_o]) in (* Add the optimised head to the end of the list of already-optimised functions *)
    h_o::(optimise_program_with_updating_program os' tl) (* Recurse with the tail, adding that to the end of the optimised functions. *)
	| [] -> []

and optimise_program i_program = optimise_program_with_updating_program [] i_program
