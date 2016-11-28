%{
open Parser_types
%}

(* Token Declaration *)

%token <int> CONST_INT
%token PLUS
%token MINUS
%token TIMES
%token DIVIDE

%token WHILE
%token IF
%token ELSE
%token READINT
%token PRINTINT
%token LET
%token NEW
%token IN

%token TRUE
%token FALSE

%token CURLY_OPEN
%token CURLY_CLOSE
%token PARENTHESIS_OPEN
%token PARENTHESIS_CLOSE

%token SEQ
%token ASG

%token DOT

%token LEQ
%token GEQ
%token EQUAL
%token NOTEQ

%token AND
%token OR
%token NOT

%token DEREF
%token REF

%token STRUCT

%token <string> STRING_VALUE
%token <string> IDENTIFIER

%token COMMA

%token EOF

(* Priorities and Associativity *)
%left SEQ
%left ASG
%left LEQ
%left GEQ
%left EQUAL
%left NOTEQ
%left AND
%left OR
%left NOT

%left PLUS
%left MINUS
%left TIMES
%left DIVIDE

%nonassoc REF
%nonassoc DEREF

%start <Parser_types.program> top
%%

top :
| p = prog; EOF 	{ p }

prog :
| f = func; EOF 	{ ([f], []) }
| s = strct; EOF 	{ ([], [s]) }
| f = func; p	= prog	{ match p with (fs, ss) -> (f :: fs, ss) }
| s = strct; p = prog	{ match p with (fs, ss) -> (fs, s :: ss) }

func :
	funcid = IDENTIFIER;
	PARENTHESIS_OPEN; args = arglist; PARENTHESIS_CLOSE;
	e = scoped_exp					{ Myfunc (funcid, args, e) }

strct : STRUCT; IDENTIFIER; CURLY_OPEN; ms = memlist; CURLY_CLOSE		{ ($2, ms) }

arglist : args = separated_list(COMMA, IDENTIFIER)	{ args }

explist : PARENTHESIS_OPEN; args = separated_list(COMMA, exp); PARENTHESIS_CLOSE { args }

scoped_exp : CURLY_OPEN; e = exp; CURLY_CLOSE 		{ Scope (e) }

memlist :
	| md = memdef											{ [md] }
	| md = memdef; SEQ; ml = memlist	{	md :: ml }

memdef : IDENTIFIER; ASG; e = exp		{ ($1, e) }

exp :
	(* Operators *)
	| e = exp; PLUS;  f = exp   		{ Operator (Plus, e, f) }
	| e = exp; MINUS; f = exp  			{ Operator (Minus, e, f) }
	| e = exp; DIVIDE;  f = exp 		{ Operator (Divide, e, f) }
	| e = exp; TIMES; f = exp  			{ Operator (Times, e, f) }
	| e = exp; LEQ;  f = exp   			{ Operator (Leq, e, f) }
	| e = exp; GEQ; f = exp  				{ Operator (Geq, e, f) }
	| e = exp; EQUAL;  f = exp 			{ Operator (Equal, e, f) }
	| e = exp; NOTEQ; f = exp  			{ Operator (Noteq, e, f) }
	| e = exp; AND; f = exp  				{ Operator (And, e, f) }
	| e = exp; OR;  f = exp 				{ Operator (Or, e, f) }
	| NOT; e = exp;		  						{ Operator_unary (Not, e) }

	(* I/O *)
	| READINT; PARENTHESIS_OPEN; PARENTHESIS_CLOSE	{ Readint }
	| PRINTINT;
		PARENTHESIS_OPEN;
		e = exp;
		PARENTHESIS_CLOSE							{ Printint (e) }

	(* Misc *)
	| CONST_INT		 									{ Const_int ($1) }
	| TRUE		 											{ Const_bool (true) }
	| FALSE		 											{ Const_bool (false) }
	| STRING_VALUE									{ Const_string ($1) }
	| STRUCT; IDENTIFIER						{ Const_struct ($2) }
	| IDENTIFIER; DOT; IDENTIFIER		{ Memaccess (Identifier ($1), Identifier($3)) }
	| IDENTIFIER		 								{ Identifier ($1) }
	| REF; e = exp;									{ Ref (e) }
	| DEREF; e = exp;								{ Deref (e) }
	| e = exp; SEQ; f = exp					{ Seq (e, f) }
	| e = scoped_exp; f = exp				{ Seq (e, f) }
	| e = exp; ASG; f = exp					{ Asg (e, f) }
	| LET; id = IDENTIFIER; ASG; e = exp;
		IN; f = exp										{ Let (id, e, f) }
	| NEW; id = IDENTIFIER; ASG; e = exp;
		IN; f = exp										{ New (id, e, f) }
	| str = IDENTIFIER; args = explist		{ Application (Identifier(str), args) }

	(* Conditionals *)
	| IF; PARENTHESIS_OPEN;	b = exp; PARENTHESIS_CLOSE;
		e = scoped_exp;
		ELSE;
		f = scoped_exp								{ If (b, e, f) }

	| WHILE;
		PARENTHESIS_OPEN;
		b = exp;
		PARENTHESIS_CLOSE;
		e = scoped_exp								{ While (b, e) }
