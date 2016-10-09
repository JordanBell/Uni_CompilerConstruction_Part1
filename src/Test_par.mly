%token <int> INT
%token PLUS
%token TIMES
%token EOF
%left PLUS
%left TIMES
%start <int> top
%%

top :
| e = exp; EOF { e }



exp:
| i = INT 		    { i }
| e = exp; PLUS;  f = exp   { e + f }
| e = exp; TIMES; f = exp 4 { e * f }
