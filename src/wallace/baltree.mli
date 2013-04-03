(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace/baltree.mli,v 1.9 2000/02/11 16:15:47 fpottier Exp $ *)

(* This sub-module offers height-balanced binary trees, approximately as found in O'Caml's standard library. *)

module Height : GSet.S

(* This sub-module offers weight-balanced binary trees, approximately as found in Adams' 1993 paper. *)

module Weight : GSet.S

