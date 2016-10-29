open Parser_types
open Parser_evaluator
open Printf

let dummy_store =
{
	decl_funcs = ref [];
  decl_ids   = ref [];
  tbl_refs   = ref (Hashtbl.create 1);
}

let apply_to_subexpressions p e = match e with
  | Operator (op, e, f) -> Operator(op, p e, p f)
  | Operator_unary (op, e) -> Operator_unary(op, p e)
  | If (b, e, Empty) -> If (p b, p e, Empty);
  | If (b, e, f) -> If (p b, p e, p f);
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
  | Const_int _ | Const_bool _ | Const_string _ -> e
  | Identifier _ -> e
  | Empty -> e

let rec optimise_exp e = match e with
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

  | If(Const_bool b, e, f) -> if b then optimise_exp e else optimise_exp f
  | If(Const_int i, e, f) -> if (i != 0) then optimise_exp e else optimise_exp f

  | _ -> apply_to_subexpressions optimise_exp e

let rec optimise_function func =
  match func with Myfunc (str, args, e) ->
    Myfunc (str, args, optimise_exp e)
