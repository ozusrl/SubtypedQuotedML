(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace/trieSetMap.mli,v 1.4 2000/02/11 16:15:54 fpottier Exp $ *)

(* This module, parameterized over an implementation of sets, provides maps whose keys are sets. As its name implies,
   its implementation uses tries. *)

module Make (X : sig

  type 'a t
  type 'a ordering = 'a -> 'a -> int

  val make2: 'a ordering -> 'a -> 'a -> 'a t
  val add: 'a ordering -> 'a -> 'a t -> 'a t
  val union: 'a ordering -> 'a t -> 'a t -> 'a t
  val iter: ('a -> unit) -> 'a t -> unit
  val iterator: 'a t -> unit -> 'a option

end) : LeafSetMap.S

