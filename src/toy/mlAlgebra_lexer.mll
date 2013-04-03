(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/toy/mlAlgebra_lexer.mll,v 1.10 2000/02/11 16:16:34 fpottier Exp $ *)

(* Lexing type expressions. *)

{

open MlAlgebra_parser

exception Error of string * int * int

let fail lexbuf message =
  raise (Error(message, Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf))  

}

let blank = [' ' '\010' '\013' '\009' '\012']
let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let leadingchar = ['a'-'z' '\223'-'\246' '\248'-'\255' '_' 'A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar = 
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']

rule token = parse
    blank +
      { token lexbuf }
  | "{}" { Record }
  | "[]" { Variant }
  | "{" { LBrace }
  | "[" { LBracket }
  | "(" { LParen }
  | "'" { Quote }
  | "if" { If }
  | "then" { Then }
  | "}" { RBrace }
  | ")" { RParen }
  | "]" { RBracket }
  | "<" { Less }
  | "->" { Arrow }
  | "*" { Star }
  | "," { Comma }
  | ":" { Colon }
  | ";" { Semi }
  | "0" { Bottom }
  | "\\" { Urow }
  | "RAbs" { RAbsent }
  | "R0" { RBottom }
  | "REither" { REither }
  | "RPre" { RPresent }
  | "R1" { RTop }
  | "VAbs" { VAbsent }
  | "VPre" { VPresent }
  | "V1" { VTop }
  | "1" { Top }
  | "int" { Int }
  | "unit" { Unit }
  | "bool" { Bool }
  | "float" { Float }
  | "char" { Char }
  | "string" { String }
  | "ref" { Ref }
  | "vect" { Vect }
  | leadingchar identchar *
      { Ident (Lexing.lexeme lexbuf) }
  | eof { EOF }
  | _
      { fail lexbuf "Illegal character" }

