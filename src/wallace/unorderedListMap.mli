(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace/unorderedListMap.mli,v 1.3 2000/02/11 16:15:55 fpottier Exp $ *)

(* This is a trivial implementation of general-purpose maps, based on unordered lists. Its [add] operation works in
   constant time, but its [lookup] function is very inefficient, since its complexity is not even linear in the number
   of bindings in the map; it is, in fact, linear in the number of previous [add] operations. *)

include GMap.Additive

