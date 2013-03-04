(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/toy/compile.mli,v 1.5 2000/02/11 16:16:32 fpottier Exp $ *)

(* This module, boldly called a ``compiler'', simplifies the external expression language, so as to show only a
   simple, core language to our type-checker. This transformation could simply be called ``de-sugaring''. *)

val expression: ToySyntax.expression -> InternalSyntax.expression

(* The following exception is raised when a matching is heterogeneous, that is, when it involves two incompatible
   forms of structure, such as a pair and a record. *)

exception HeterogeneousMatching

(* The following exception is raised when a matching is non-rectangular, i.e. when two lines have a different number
   of columns. *)

exception NonRectangularMatching

(* The following exception is raised when a pattern is illegal, i.e. it either binds identifiers inside an
   alternative, or is non-linear. The exception's argument is an explanation message. *)

exception IllegalPattern of string

