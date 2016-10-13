%{
open Parser_types
%}

(* Token Declaration *)

%token <int> CONST
%token PLUS
%token MINUS	
%token TIMES
%token DIVIDE

%token WHILE
%token IF
%token ELSE
%token READINT
%token PRINTINT
(*%token LET
%token NEW
%token IN*)

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

%token TYPE_INT
%token <string> IDENTIFIER

%token COMMA

%token EOF

(* Priorities and Associativity *)
%left IF
%left ELSE
%right READINT
%right PRINTINT
(*%left IN*)

%right PARENTHESIS_OPEN (* Note: Parenthesis does not currently work to change the order of operations *)
%left PARENTHESIS_CLOSE

%left SEQ
(*%right LET
%right NEW*)
%left ASG
%left LEQ
%left GEQ
%left EQUAL
%left NOTEQ
%left AND
%left OR
%left NOT

%right TYPE_INT

%left CURLY_OPEN
%left CURLY_CLOSE

%left PLUS
%left MINUS
%left TIMES
%left DIVIDE

%start <Parser_types.program> top
%%

top :
| p = prog; EOF 	{ p }

prog : 
| f = func; EOF 	{ [f] }
| f = func; p = prog	{ f :: p }

func : 
	funcid = IDENTIFIER; 
	PARENTHESIS_OPEN; args = arglist; PARENTHESIS_CLOSE; 
	e = exp					{ Myfunc (funcid, args, e) }

arglist : 
	args = separated_list(COMMA, IDENTIFIER)	{ args }

exp : 
	(* Operators *)
	| e = exp; PLUS;  f = exp   			{ Operator (Plus, e, f) }
	| e = exp; MINUS; f = exp  			{ Operator (Minus, e, f) }
	| e = exp; DIVIDE;  f = exp 			{ Operator (Divide, e, f) }
	| e = exp; TIMES; f = exp  			{ Operator (Times, e, f) }
	| e = exp; LEQ;  f = exp   			{ Operator (Leq, e, f) }
	| e = exp; GEQ; f = exp  			{ Operator (Geq, e, f) }
	| e = exp; EQUAL;  f = exp 			{ Operator (Equal, e, f) }
	| e = exp; NOTEQ; f = exp  			{ Operator (Noteq, e, f) }
	| e = exp; AND; f = exp  			{ Operator (And, e, f) }
	| e = exp; OR;  f = exp 			{ Operator (Or, e, f) }
	| NOT; e = exp;		  			{ Operator_unary (Not, e) }

	(* I/O *)
	| READINT; PARENTHESIS_OPEN; PARENTHESIS_CLOSE	{ Readint }
	| PRINTINT; 
		PARENTHESIS_OPEN; 
		e = exp;
		PARENTHESIS_CLOSE			{ Printint (e) }

	(* Misc *)
	| CONST		 				{ Const ($1) }
	| IDENTIFIER	 				{ Identifier ($1) }
	| CURLY_OPEN; e = exp; CURLY_CLOSE 		{ Scope (e) }
	| e = exp; SEQ; f = exp				{ Seq (e, f) }
	| e = exp; ASG; f = exp				{ Asg (e, f) }
	| TYPE_INT; IDENTIFIER; ASG; e = exp;
		SEQ; f = exp				{ New ($2, e, f) } 
	(*| LET; e = exp; IN; f = exp;			{ Let (e, f) }*)
	(*| NEW; e = exp; IN; f = exp;			{ New (e, f) } *)
	| e = exp; 
		PARENTHESIS_OPEN; 
		f = exp; 
		PARENTHESIS_CLOSE			{ Application (e, f) }

	(* Conditionals *)
	| IF; 
		PARENTHESIS_OPEN; 
		b = exp; 
		PARENTHESIS_CLOSE; 
		e = exp; 
		ELSE; 
		f = exp  				{ If (b, e, f) }

	| WHILE; 
		PARENTHESIS_OPEN; 
		b = exp; 
		PARENTHESIS_CLOSE; 
		e = exp;				{ While (b, e) }


