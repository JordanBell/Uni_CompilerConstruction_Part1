open Printf
open Parser_printer
open Parser_par
open Parser_lex
open Parser_types

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
	| Const_int (_) | Const_bool (_) | Const_string (_) | Const_struct (_) -> 1
	| Identifier (str) -> 1
	| Memaccess (e, f) -> 1
	| Empty -> 0

let concat_both (a1, a2) (b1, b2) = (a1@b1, a2@b2)

let rec parse_file filename is_verbose =
  (* Print the initial message for processing the file *)
  (if is_verbose then
    printf "\n***** Parsing START on file: %s\n" filename
  else
    printf "%s... " filename);

  (* Read in all of the lines *)
  let i_lines = read_all filename in

  let num_lines = List.length i_lines in
  (if is_verbose then printf "*** Found %d lines\n" num_lines);
  (if is_verbose then printlines i_lines);
  if num_lines = 0 then ([], [])
  else
    (* Determine the diretory filepath this is within (used for calculating relative filepaths for included files)*)
    let slash_index = String.rindex filename '/' in
    let directory_filepath = String.sub filename 0 (slash_index + 1) in

    let lines_as_string = String.concat "\n" i_lines in
    let parsed_program = (parse_lines lines_as_string directory_filepath) in

    (if is_verbose then printf "Result:\n");
    (if is_verbose then print_parse_result (match parsed_program with (funcs, structs) -> funcs));
    (if is_verbose then printf "***** Parsing FINISHED on file: %s\n\n" filename);
    parsed_program

and parse_lines i_str directory_filepath =
	let i_buf = Lexing.from_string i_str in
	try
    let parse_result = Parser_par.top Parser_lex.read i_buf in
    merge_included_programs parse_result directory_filepath
	with
		| SyntaxError str -> printf "Parsing Syntax error: %s\n" str; ([], [])
		| Parser_par.Error -> printf "Parser failed\n"; ([], [])

and merge_included_programs (prog, is) dir_filepath = (* Takes a program and its included filenames, and returns a program which has had the parsed contents of those files concatenated to it *)
  match is with
    | [] -> prog
    | h::tl ->
      let inc_filename = dir_filepath ^ h in
      printf "Include[";
      let prog_inc = parse_file inc_filename false in (* Pass is_verbose = false for simplicity's sake *)
      let prog_merged = concat_both prog prog_inc in
      printf "] ";
      merge_included_programs (prog_merged, tl) dir_filepath
