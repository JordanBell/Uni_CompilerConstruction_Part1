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
%token LET
%token NEW
%token IN

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

%token DEREF
%token REF

%token <string> IDENTIFIER

%token COMMA

%token EOF

(* Priorities and Associativity *)
%left ELSE

%right PARENTHESIS_OPEN (* Note: Parenthesis does not currently work to change the order of operations *)
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

%right REF
%right DEREF

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
	e = scoped_exp					{ Myfunc (funcid, args, e) }

arglist : args = separated_list(COMMA, IDENTIFIER)	{ args }

explist : PARENTHESIS_OPEN; args = separated_list(COMMA, exp); PARENTHESIS_CLOSE { args } 

scoped_exp : CURLY_OPEN; e = exp; CURLY_CLOSE 		{ Scope (e) }

scoped_exp_optional :
	| e = exp					{ e }
	| e = scoped_exp				{ e }

else_trail :
	| ELSE; b = exp; 				{ b }
	| SEQ 						{ Empty }

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
		PARENTHESIS_CLOSE					{ Printint (e) }

	(* Misc *)
	| CONST		 							{ Const ($1) }
	| IDENTIFIER	 						{ Identifier ($1) }
	| REF; e = exp;							{ Ref (e) }
	| DEREF; e = exp;						{ Deref (e) }
	| e = exp; SEQ; f = exp					{ Seq (e, f) }
	| e = scoped_exp; f = exp				{ Seq (e, f) }
	| e = exp; ASG; f = exp					{ Asg (e, f) }
	| LET; id = IDENTIFIER; ASG; e = exp;
		IN; f = exp							{ Let (id, e, f) } 
	| NEW; id = IDENTIFIER; ASG; e = exp;
		IN; f = exp							{ New (id, e, f) } 
	| str = IDENTIFIER; args = explist		{ Application (Identifier(str), args) }

	(* Conditionals *)
	| IF; PARENTHESIS_OPEN;	b = exp; PARENTHESIS_CLOSE; 
		e = scoped_exp; 
		ELSE; 
		f = scoped_exp						{ If (b, e, f) }

	| WHILE; 
		PARENTHESIS_OPEN; 
		b = exp; 
		PARENTHESIS_CLOSE; 
		e = scoped_exp						{ While (b, e) }


