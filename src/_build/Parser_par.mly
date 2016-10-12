%token <int> CONST
%token PLUS
%token MINUS	
%token TIMES
%token DIVIDE
%token ASG

%token LEQ
%token GEQ
%token EQUAL
%token NOTEQ

%token AND
%token OR
%token NOT

%token EOF

%left PLUS
%left MINUS
%left TIMES
%left DIVIDE
%left ASG
%start <int> top
%%

type expression =
  | Seq of expression * expression (* e; e *)
  | While of expression * expression (* while e do e *)
  | If of expression * expression * expression (* if e do e else e *)
  | Asg of expression * expression (* e := e *)
  | Deref of expression (* !e *)
  | Operator of opcode * expression * expression (* e + e *)
  | Application of expression * expression (* e(e) *)
  | Const of int (* 7 *)
  | Readint (* read_int () *)
  | Printint of expression (* print_int (e) *)
  | Identifier of string (* x *)
  | Let of string * expression * expression (* let x = e in e *)
  | New of string * expression * expression (* new x = e in e *)
type fundef = string * string list * expression
type program = fundef list




top :
| e = exp; EOF { e }

exp:
| i = INT 		    	{ i }
| e = exp; PLUS;  f = exp   	{ e + f }
| e = exp; MINUS; f = exp 4 	{ e - f }
| e = exp; DIVIDE;  f = exp 	{ e / f }
| e = exp; TIMES; f = exp 4 	{ e * f }
| ASG; e = exp; f = exp		{ ASG e f }
