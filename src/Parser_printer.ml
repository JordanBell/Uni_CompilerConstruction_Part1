open Printf
open Parser_types
open Parser_lex

(* String Conversions *)
let string_of_opcode = function
	| Plus -> "Plus"
	| Minus -> "Minus"
	| Times -> "Times"
	| Divide -> "Divide"
	| And -> "And"
	| Or -> "Or"
	| Geq -> "Geq"
	| Leq -> "Leq"
	| Equal -> "Equal"
	| Noteq -> "Noteq"

let rec string_of_eval_result er = match er with
	| Int (i) -> string_of_int i
	| Bool (b) -> if b then "true" else "false"
	| String (s) -> s
	| Struct_data (data_tbl) -> "[struct]"
	| Identifier (s) -> "ID:" ^ s
	| Unit -> "Unit"

let rec string_of_eval_result_type er = match er with
	| Int (i) -> "int"
	| Bool (b) -> "bool"
	| String (s) -> "string"
	| Struct_data (data_tbl) -> "struct"
	| Identifier (s) -> "Identifier"
	| Unit -> "Unit"

let string_of_opcode_unary = function
	| Not -> "Not"

let rec string_of_args = function
	| [] -> "void"
	| [h] -> h
	| h::tl -> h ^ ", " ^ string_of_args tl

let rec print_expression expr acc =
	let indent_str = String.make (acc * 2) ' ' in
	let indent_str_next = String.make (2 * (acc+1)) ' ' in
	match expr with
		| Operator (op, e, f) ->
			let opcode_str = string_of_opcode op in
			printf "%sOperator\n%s(\n" indent_str indent_str;
			printf "%s%s" indent_str_next opcode_str; 	printf ",\n";			(* Print opcode *)
			print_expression e (acc+1); 			printf ",\n"; 			(* Print first expr *)
			print_expression f (acc+1); 							(* Print second expr *)
			printf "\n%s)" indent_str

		| Operator_unary (op, e) ->
			let opcode_str = string_of_opcode_unary op in
			printf "%sOperator_unary\n%s(\n" indent_str indent_str;
			printf "%s%s" indent_str_next opcode_str; 	printf ",\n";			(* Print opcode *)
			print_expression e (acc+1); 							(* Print expr *)
			printf "\n%s)" indent_str

		| If (b, e, Empty) ->
			printf "%sIf\n%s(\n" indent_str indent_str;
			print_expression b (acc+1); 		 	printf "\n%sDo\n" indent_str;	(* Print Conditional expr *)
			print_expression e (acc+1);  							(* Print Do expr *)
			printf "\n%s)" indent_str

		| If (b, e, f) ->
			printf "%sIf\n%s(\n" indent_str indent_str;
			print_expression b (acc+1); 		 	printf "\n%sDo\n" indent_str;	(* Print Conditional expr *)
			print_expression e (acc+1); 			printf "\n%sElse\n" indent_str; (* Print do expr *)
			print_expression f (acc+1); 							(* Print else expr *)
			printf "\n%s)" indent_str

		| While (b, e) ->
			printf "%sWhile\n%s(\n" indent_str indent_str;
			print_expression b (acc+1);			printf ",\n"; 			(* Print first expr *)
			print_expression e (acc+1);							(* Print second expr *)
			printf "\n%s)" indent_str

		| Asg (e, f) ->
			printf "%sAsg\n%s(\n" indent_str indent_str;
			print_expression e (acc+1);			printf ",\n"; 			(* Print first expr *)
			print_expression f (acc+1);							(* Print second expr *)
			printf "\n%s)" indent_str

		| Seq (e, f) ->
			printf "%sSeq\n%s(\n" indent_str indent_str;
			print_expression e (acc+1);			printf ",\n"; 			(* Print first expr *)
			print_expression f (acc+1);							(* Print second expr *)
			printf "\n%s)" indent_str

		| Application (e, fs) ->
			printf "%sApplication\n%s(\n" indent_str indent_str;
			print_expression e (acc+1);			printf ",\n"; 			(* Print first expr *)
			print_expression_list fs (acc+1)

		| Let (str, e, f) ->
			printf "%sLet\n%s(\n" indent_str indent_str;
			printf "%s%s\n" indent_str_next str; 	(* Print first expr *)
			print_expression e (acc+1);			printf "\n%sIn\n" indent_str; 	(* Print first expr *)
			print_expression f (acc+1);							(* Print second expr *)
			printf "\n%s)" indent_str

		| New (str, e, f) ->
			printf "%sNew %s = \n" indent_str str;
			print_expression e (acc+1);			printf "\n%sIn\n" indent_str; 	(* Print first expr *)
			print_expression f (acc+1);							(* Print second expr *)
			printf "\n%s)" indent_str

		| Scope (e) ->
			printf "%sScope\n%s(\n" indent_str indent_str;
			print_expression e (acc+1);
			printf "\n%s)" indent_str

		| Deref (e) ->
			printf "%sDeref\n%s(\n" indent_str indent_str;
			print_expression e (acc+1);
			printf "\n%s)" indent_str

		| Ref (e) ->
			printf "%sRef\n%s(\n" indent_str indent_str;
			print_expression e (acc+1);
			printf "\n%s)" indent_str

		| Printint (e) ->
			printf "%sPrintint\n%s(\n" indent_str indent_str;
			print_expression e (acc+1);
			printf "\n%s)" indent_str

		| Readint -> printf "%sReadint" indent_str

		| Const_int (i) -> printf "%sConst %d" indent_str i
		| Const_bool (b) -> printf "%sConst %s" indent_str (if b then "true" else "false")
		| Const_string (s) -> printf "%sConst \"%s\"" indent_str s
		| Const_struct (s) -> printf "%sConst Struct \"%s\"" indent_str s

		| Identifier (str) -> printf "%sIdentifier %s" indent_str str

		| Memaccess (e, f) ->
			printf "%sMemaccess\n%s(\n" indent_str indent_str;
			print_expression e (acc+1);			printf ",\n"; 			(* Print first expr *)
			print_expression f (acc+1);							(* Print second expr *)
			printf "\n%s)" indent_str

		| Empty -> printf "%sEmpty" indent_str
and print_expression_list es acc =
	let indent_str = String.make (acc * 2) ' ' in
	match es with
		| [] -> ()
		| e::[] -> print_expression e (acc+1); ()
		| e::tl -> print_expression e (acc+1); printf "\n%s)" indent_str; print_expression_list tl acc

let rec printlines = function
	| [] -> ()
	| h::tl -> printf "%s\n" h; printlines tl

let print_fundef (Myfunc (funcid, args, e)) =
	printf "%s (%s)\n" funcid (string_of_args args);
	print_expression e 0;
	printf "\n"

let rec print_program = function
	| [] -> ()
	| h::tl ->
		print_fundef h;
		printf "\n";
		print_program tl

let print_parse_result parse_result = printf "*** Result: \n"; print_program parse_result (* NOTE: This actually only prints the functions, not the struct definitions *)

let rec print_eval_result store e_eval_result =
	(* Print the resulting eval_result *)
	match e_eval_result with
		| Int (i) -> printf "%d\n" i
		| Bool (b) -> if b then printf "true\n" else printf "false\n"
		| String (s) -> printf "%s\n" s
		| Struct_data (s) -> printf "%s\n" (string_of_eval_result e_eval_result)
		| Identifier (str) -> (*failwith "Evaluated to an identifier reference. Suggested that any reference is dereferenced when printing"*)
			(try
				let definition = List.assoc str !(store.decl_ids) in
				(match definition with
					| Const (er)	-> print_eval_result store er
					| Var (addr) ->
						try print_eval_result store (Hashtbl.find !(store.tbl_refs) addr)
						with Not_found -> failwith ("Identifier \'" ^ str ^ "\' not declared."))
			with Not_found -> failwith (str ^ " not found. This may because the environment has been destroyed before printing, and the new-bound identifier's reference is therefore lost."))
		| Unit -> printf "Unit\n"

let rec print_args arg_pairs = match arg_pairs with
	| h::tl ->
		printf "%s = " (fst h);
		let definition = snd h in
		(match definition with
			| Const (er) -> (match er with
				| Int (i) -> printf "%d\n" i
				| Bool (b) -> if b then printf "true\n" else printf "false\n"
				| String (s) -> printf "%s\n" s
				| _ -> ())
			| _ -> ());
		print_args tl
	| [] -> ()
