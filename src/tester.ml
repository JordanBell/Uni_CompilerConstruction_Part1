let rec read_to_empty buf = 
	let i_str = read_line () in
	if s = "" then buf
	else (Buffer.add_string buf s;
	      Buffer.add_string buf "\n";
	      read_to_empty buf)

let _ = 
	read_to_empty (Buffer.create 1)
	|> Buffer.contents
	|> Lexing.from_string
	|> Test_par.top Test_lex.read
	|> List.map string_of_int
	|> String.concat ",\n"
	|> print_endline
