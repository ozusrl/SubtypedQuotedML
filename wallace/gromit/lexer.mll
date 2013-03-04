(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/gromit/lexer.mll,v 1.5 2000/02/11 16:15:36 fpottier Exp $ *)

(* Gromit's lexer definition, inspired from Objective Caml's (with huge simplifications). *)

{

open Parser

(* The table of keywords. *)

let keyword_table =
  let table = Hashtbl.create 149 in
  List.iter (fun (x, y) -> Hashtbl.add table x y) [
    "begin", BEGIN;
    "constructor", CONSTRUCTOR;
    "contravariant", CONTRAVARIANT;
    "covariant", COVARIANT;
    "end", END;
    "lattice", LATTICE;
    "let", LET;
    "printed", PRINTED;
    "priority", PRIORITY;
    "signature", SIGNATURE
  ];
  table

(* Dealing with comments. *)

let comment_depth = ref 0
let comment_start_pos = ref 0

(* Reporting errors. *)

type error =
    Illegal_character
  | Unterminated_comment

exception Error of error * int * int

let report_error = function
    Illegal_character ->
      "illegal character"
  | Unterminated_comment ->
      "comment not terminated"

}

let blank = [' ' '\010' '\013' '\009' '\012']
let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar = 
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']

rule token = parse
    blank +
      { token lexbuf }
  | lowercase identchar *
      { let s = Lexing.lexeme lexbuf in
          try
            Hashtbl.find keyword_table s
          with Not_found ->
            LIDENT s }
  | uppercase identchar *
      { UIDENT(Lexing.lexeme lexbuf) }       (* No capitalized keywords *)
  | "(*"
      { comment_depth := 1;
        comment_start_pos := Lexing.lexeme_start lexbuf;
        comment lexbuf;
        token lexbuf }
  | "<" { LESS }
  | ">" { MORE }
  | "," { COMMA }
  | "{" { LBRACE }
  | "[" { LBRACK }
  | "(" { LPAREN }
  | "'" { QUOTE }
  | "}" { RBRACE }
  | "]" { RBRACK }
  | ")" { RPAREN }
  | eof { EOF }
  | _
      { raise (Error(Illegal_character,
                     Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)) }

and comment = parse
    "(*"
      { comment_depth := succ !comment_depth; comment lexbuf }
  | "*)"
      { comment_depth := pred !comment_depth;
        if !comment_depth > 0 then comment lexbuf }
  | eof
      { raise (Error (Unterminated_comment,
                      !comment_start_pos, !comment_start_pos+2)) }
  | _
      { comment lexbuf }
