(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/toy/token.mli,v 1.2 2000/02/11 16:16:34 fpottier Exp $ *)

(* These modules define how types are printed, by giving concrete meaning to abstract tokens. Two output formats are
   given: regular text and \TeX. *)

open Print

module Text : TokenPrinter with type token = MlAlgebra.Print.token

module TeX : TokenPrinter with type token = MlAlgebra.Print.token
  
