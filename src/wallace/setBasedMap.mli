(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace/setBasedMap.mli,v 1.2 2000/02/11 16:15:51 fpottier Exp $ *)

(* This module turns any implementation of general-purpose sets into an implementation of general-purpose maps. *)

module Make (X : GSet.S) : GMap.S

