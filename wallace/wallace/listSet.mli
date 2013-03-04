(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace/listSet.mli,v 1.3 2000/02/11 16:15:49 fpottier Exp $ *)

(* This module provides a naïve, list-based implementation of sets. It might be useful when dealing with very small
   sets, because it has very little overhead. *)

include GSet.S

