%{
open Parser_types
%}

(* Token Declaration *)

%token <int> CONST
%token PLUS
%token MINUS	
%token TIMES
%token DIVIDE

%token IF
%token ELSE
%token CURLY_OPEN
%token CURLY_CLOSE
%token PARENTHESIS_OPEN
%token PARENTHESIS_CLOSE

%token SEQ
%token ASG

%token LEQ
%token GEQ
%token EQUAL
%token NOTEQ

%token AND
%token OR
%token NOT

%token EOF

(* Priorities and Associativity *)
%left IF
%left ELSE

%left PARENTHESIS_OPEN (* Note: Parenthesis does not currently work to change the order of operations *)
%left PARENTHESIS_CLOSE

%left SEQ
%left ASG
%left LEQ
%left GEQ
%left EQUAL
%left NOTEQ
%left AND
%left OR
%left NOT

%left CURLY_OPEN
%left CURLY_CLOSE

%left PLUS
%left MINUS
%left TIMES
%left DIVIDE

%start <Parser_types.expression> top
%%

top :
| e = exp; EOF { e }

exp : 
(* Operators *)
| e = exp; PLUS;  f = exp   		{ Operator (Plus, e, f) }
| e = exp; MINUS; f = exp  		{ Operator (Minus, e, f) }
| e = exp; DIVIDE;  f = exp 		{ Operator (Divide, e, f) }
| e = exp; TIMES; f = exp  		{ Operator (Times, e, f) }
| e = exp; LEQ;  f = exp   		{ Operator (Leq, e, f) }
| e = exp; GEQ; f = exp  		{ Operator (Geq, e, f) }
| e = exp; EQUAL;  f = exp 		{ Operator (Equal, e, f) }
| e = exp; NOTEQ; f = exp  		{ Operator (Noteq, e, f) }
| e = exp; AND; f = exp  		{ Operator (And, e, f) }
| e = exp; OR;  f = exp 		{ Operator (Or, e, f) }
| e = exp; NOT; f = exp  		{ Operator (Not, e, f) }

(* Misc *)
| CONST		 			{ Const ($1) }
| e = exp; SEQ; f = exp			{ Seq (e, f) }
| e = exp; ASG; f = exp			{ Asg (e, f) }
| CURLY_OPEN; e = exp; CURLY_CLOSE 	{ Scope (e) }

(* Conditionals *)
| IF; PARENTHESIS_OPEN; b = exp; PARENTHESIS_CLOSE; 
	e = exp; 
	ELSE; 
	f = exp  			{ If (b, e, f) }


