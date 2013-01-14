%{
  open Common
%}

%token EOF DOT FUN LP RP LB RB UNBOX RUN LET IN SEMI LT GT WITH COMMA ARROW
%token EQ PLUS MINUS MULT DIV FIX IF THEN ELSE LIFT LBRACK RBRACK
%token <string> ID
%token <int>    INT
%token <bool>   BOOL
%start main
%type <Common.exp> main

/* Assoc and precedence definitions */

%nonassoc ELSE IN ARROW
%right COMMA
%nonassoc EQ
%left DIV MULT
%left PLUS MINUS
%nonassoc WITH
%nonassoc FUN FIX LET LP LBRACK LT LB IF ID INT BOOL
%left APP
%nonassoc UNBOX RUN LIFT
%nonassoc DOT

%%

main:
    exp SEMI { $1 }

dec:
    ID EQ exp        { Valbind($1,$3) }

exp:
    ID                          { IdE $1 }
  | INT                         { ConstE(CInt $1) }
  | BOOL                        { ConstE(CBool $1) }
  | FUN ID ARROW exp            { AbsE(Abs($2, $4)) }
  | FIX ID ID ARROW exp         { FixE($2, Abs($3,$5)) }
  | LET dec IN exp              { LetInE($2, $4) }
  | exp PLUS exp                { BinaryE(Plus($1, $3)) }
  | exp MINUS exp               { BinaryE(Minus($1, $3)) }
  | exp MULT exp                { BinaryE(Mult($1, $3)) }
  | exp DIV exp                 { BinaryE(Div($1, $3)) }
  | exp EQ exp                  { BinaryE(Equals($1, $3)) }
  | exp exp %prec APP           { AppE($1,$2) }
  | LT exp GT                   { BoxE $2 }
  | UNBOX exp                   { UnboxE $2 }
  | RUN exp                     { RunE $2 }
  | LIFT exp                    { LiftE $2 }
  | LP exp RP                   { $2 }
  | IF exp THEN exp ELSE exp    { CondE( [ ($2, $4); ((ConstE (CBool true)), $6) ] ) }

%%
