(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace/teXPrinter.ml,v 1.6 2000/02/11 16:15:53 fpottier Exp $ *)

module Make
    (Core: Core.S)

= struct

  open Printf
  open Core

  let signs =
    ref true

  type sign = Core.sign

  let variable sign index =
    let infix = if !signs then
      match sign with
      | Bipolar -> "^\pm"
      | Positive -> "^+"
      | Negative -> "^-"
      | Neutral -> ""
    else "" in
    let n = string_of_int index in
    print_string ("\alpha" ^ infix ^ "_" ^ (if index < 10 then n else "{" ^ n ^ "}"))

  let box p =
    p()

  let parentheses p =
    printf "(";
    p();
    printf ")"

  let angle p =
    printf "\\langle";
    p();
    printf "\\rangle"

  let comma () =
    printf ", "

  let colon () =
    printf ": "

  let semi () =
    printf "; "

  let less () =
    printf "\\leq"

  let conditional hypothesis conclusion =
    printf "\\texttt{if }";
    hypothesis();
    printf "\\texttt{ then }";
    conclusion()

  let newline () =
    printf "\\\\\n"

  let label label =
    printf "\\texttt{%s}" label

  let urow () =
    printf "\urow"

end

