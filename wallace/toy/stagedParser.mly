%{
  open StagedCommon

  let rec seqToCons e =
    match e with
    | SeqE(left, right) -> AppE (AppE (IdE "::", left), seqToCons(right))
    | _ -> AppE (AppE (IdE "::", e), EmpLstE)
%}

%token EOF DOT FUN LP RP LB RB UNBOX RUN LET IN SEMI LT GT COMMA ARROW COLON
%token EQ PLUS MINUS MULT DIV FIX IF THEN ELSE LIFT LBRACK RBRACK CONS REC ELSEIF
%token REF BANG ASSIGN DSEMI
%token <string> ID
%token <int>    INT
%token <bool>   BOOL
%start main
%type <StagedCommon.exp> main

/* Assoc and precedence definitions */

%nonassoc IN ARROW
%right SEMI
%right THEN ELSE ELSEIF
%right ASSIGN
%right CONS
%right COMMA
%nonassoc EQ
%left PLUS MINUS
%left DIV MULT
%nonassoc FUN FIX LET LP LBRACK LT LB IF ID INT BOOL
%left APP
%nonassoc UNBOX RUN LIFT
%nonassoc DOT
%right REF BANG

%%

main:
  | exp DSEMI                   { $1 }

exp:
  | ID                          { IdE $1 }
  | INT                         { ConstE (CInt $1) }
  | BOOL                        { ConstE (CBool $1) }
  | FUN func                    { AbsE $2 }
  | FIX ID func                 { FixE ($2, $3) }

  | LET dec IN exp              { LetInE ($2, $4) }
  | LET ID eqfunc IN exp        { LetInE (Valbind ($2, AbsE $3), $5) }
  | LET REC ID eqfunc IN exp    { LetInE (Valbind ($3, FixE ($3, $4)), $6) }

  | exp PLUS exp                { AppE (AppE (IdE "+", $1), $3) }
  | exp MINUS exp               { AppE (AppE (IdE "-", $1), $3) }
  | exp MULT exp                { AppE (AppE (IdE "*", $1), $3) }
  | exp DIV exp                 { AppE (AppE (IdE "/", $1), $3) }
  | exp EQ exp                  { AppE (AppE (IdE "=", $1), $3) }
  | exp CONS exp                { AppE (AppE (IdE "::", $1), $3) }
  | exp exp %prec APP           { AppE ($1, $2) }
  | LT exp GT                   { BoxE $2 }
  | UNBOX exp                   { UnboxE $2 }
  | RUN exp                     { RunE $2 }
  | LIFT exp                    { LiftE $2 }

  | LP PLUS RP                  { IdE "+" }
  | LP MINUS RP                 { IdE "-" }
  | LP MULT RP                  { IdE "*" }
  | LP DIV RP                   { IdE "/" }
  | LP EQ RP                    { IdE "=" }
  | LP CONS RP                  { IdE "::" }
  | LP exp RP                   { $2 }

  | LP exp COMMA exp RP         { PairE ($2, $4) }

  | IF exp THEN exp else_part   { IfE ($2, $4, $5) }
  | lst                         { $1 }
  | record                      { $1 }
  | exp DOT ID                  { SelectE ($1, $3) }
    /* TODO: find a better record update syntax */
  | LB LB exp DOT ID COLON exp RB RB
                                { RecUpdE ($3, $5, $7) }

  | REF exp                     { RefE $2 }
  | BANG exp                    { DerefE $2 }
  | exp ASSIGN exp              { AssignE ($1, $3) }

  | exp SEMI exp                { SeqE ($1, $3) }

dec:
  | ID EQ exp                   { Valbind ($1,$3) }

else_part:
  | ELSEIF exp THEN exp else_part
                                { IfE ($2, $4, $5) }
  | ELSE exp                    { $2 }

func:
  | ID func                     { Abs ($1, AbsE $2) }
  | ID ARROW exp                { Abs ($1, $3) }

eqfunc:
  | ID eqfunc                   { Abs ($1, AbsE $2) }
  | ID EQ exp                   { Abs ($1, $3) }

lst:
  | LBRACK exp RBRACK           { seqToCons($2) }
  | LBRACK RBRACK               { EmpLstE }

record:
  | LB RB                       { EmptyRecE }
  | LB fields RB                { $2 }

fields:
  | ID EQ exp                   { RecUpdE (EmptyRecE, $1, $3) }
  | ID EQ exp SEMI fields       { RecUpdE ($5, $1, $3) }

%%
