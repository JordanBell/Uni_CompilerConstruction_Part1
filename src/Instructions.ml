(*

Abstract traversal of an expression tree, applying an abstract instruction
opset: a collection of implementations for instructions. This is implemented
in Interpreter.ml and CodeGeneration.ml accordingly.

*)

open Parser
open Parser_lex
open Parser_types
open Parser_printer
open Parser_evaluator

let addr_counter = ref 0
let new_addr() = addr_counter := !addr_counter + 1; !addr_counter

type opset =
{
  on_op : (opcode * int * int) -> unit;
  on_st : int -> unit;
  on_ldc : int -> unit;
}

let rec run_with opset symt exp =
  let frec = run_with opset symt in
  match exp with
    | Operator (opcode, e, f) ->
      let addr1 = frec e in
      let addr2 = frec f in
      opset.on_op (opcode, addr1, addr2);
      let addr3 = new_addr() in
      opset.on_st addr3;
      addr3
    | Operator_unary (opcode, e) -> failwith "Expression not implemented: Unary Operator (NOT)"

    | Identifier (id_str) -> List.assoc id_str symt
    | Const_int n ->
      let addr = new_addr() in
      opset.on_ldc n;
      opset.on_st addr;
      addr;
    | Const_bool b ->
      (* Run the bool as an int *)
      let b_as_i = (if b then 1 else 0) in
      frec (Const_int (b_as_i))
    | Const_string (str) ->
      failwith "Const_strings not implemented for running with instruction sets."
    | Let (id_str, e, f) ->
      let addr1 = frec e in
      run_with opset ((id_str, addr1) :: symt) f
  	| Seq (e, f) ->
  		let _ = frec e in
  		frec f
    | Scope (e) -> frec e

    | Empty -> failwith "Expression not implemented: Empty"
    | If (b, e, f) -> failwith "Expression not implemented: If"
    | While (e, f) -> failwith "Expression not implemented: While"
    | Asg (e, f) -> failwith "Expression not implemented: Asg"
    | New (str, e, f) -> failwith "Expression type not implemented: New"
    | Printint (e) -> failwith "Expression not implemented: Printint"
    | Deref _ ->	failwith "Expression not implemented: Deref"
    | Ref _ -> failwith "Expression type not implemented: Ref"
    | Application (_, _) -> failwith "Expression type not implemented: Application"
    | Readint -> failwith "Expression type not implemented: Readint"
    | Const_struct _ -> failwith "Expression type not implemented: Const_struct"
    | Memaccess (_, _) -> failwith "Expression type not implemented: Memaccess"
