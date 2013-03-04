(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace/fixMap.mli,v 1.3 2000/02/11 16:15:48 fpottier Exp $ *)

(* This module accepts a general-purpose implementation of maps, and specializes its signature by fixing the type
   of keys, as well as the ordering on keys. *)

module Make (X : GMap.S) (Key : Map.OrderedType)
  : GMap.Fixed with type key = Key.t

