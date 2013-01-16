%{
  open Common
%}

%token EOF DOT FUN LP RP LB RB UNBOX RUN LET IN SEMI LT GT WITH COMMA ARROW
%token EQ PLUS MINUS MULT DIV FIX IF THEN ELSE LIFT LBRACK RBRACK CONS
%token <string> ID
%token <int>    INT
%token <bool>   BOOL
%start main
%type <Common.exp> main

/* Assoc and precedence definitions */

%right THEN ELSE
%nonassoc IN ARROW
%right COMMA
%nonassoc EQ
%left DIV MULT
%left PLUS MINUS
%nonassoc WITH
%nonassoc FUN FIX LET LP LBRACK LT LB IF ID INT BOOL
%left APP
%nonassoc UNBOX RUN LIFT
%nonassoc CONS /* TODO: make sure precedences are correct */
%nonassoc DOT

%%

main:
  | exp SEMI { $1 }

dec:
  | ID EQ exp        { Valbind($1,$3) }

exp:
  | ID                          { IdE $1 }
  | INT                         { ConstE(CInt $1) }
  | BOOL                        { ConstE(CBool $1) }
  | FUN ID ARROW exp            { AbsE(Abs($2, $4)) }
  | FIX ID ID ARROW exp         { FixE($2, Abs($3,$5)) }
  | LET dec IN exp              { LetInE($2, $4) }
  | exp PLUS exp                { AppE (AppE (IdE "+", $1), $3) }
  | exp MINUS exp               { AppE (AppE (IdE "-", $1), $3) }
  | exp MULT exp                { AppE (AppE (IdE "*", $1), $3) }
  | exp DIV exp                 { AppE (AppE (IdE "/", $1), $3) }
  | exp EQ exp                  { AppE (AppE (IdE "=", $1), $3) }
  | exp CONS exp                { AppE (AppE (IdE "::", $1), $3) }
  | exp exp %prec APP           { AppE($1,$2) }
  | LT exp GT                   { BoxE $2 }
  | UNBOX exp                   { UnboxE $2 }
  | RUN exp                     { RunE $2 }
  | LIFT exp                    { LiftE $2 }
  | LP exp RP                   { $2 }
  | IF exp THEN exp             { CondE( [ ($2, $4) ] ) }
  | IF exp THEN exp ELSE exp    { CondE( [ ($2, $4); ((ConstE (CBool true)), $6) ] ) }
  | lst                         { $1 }

lst:
  | LBRACK lst_contents RBRACK { $2 }
  | LBRACK RBRACK { EmpLstE }

lst_contents:
  | exp { AppE (AppE (IdE "::", $1), EmpLstE) }
  | exp SEMI lst_contents { AppE (AppE ((IdE "::"), $1), $3) }

%%
