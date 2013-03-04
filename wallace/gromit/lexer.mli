(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/gromit/lexer.mli,v 1.2 2000/02/11 16:15:36 fpottier Exp $ *)

(* Gromit's lexer definition. The function [token] is the lexer's main entry point, and may raise an [Error] exception
   if it encounters invalid input. The function [report_error] can be used to print an error explanation.
*)

type error

exception Error of error * int * int

val token: Lexing.lexbuf -> Parser.token
val report_error: error -> string

