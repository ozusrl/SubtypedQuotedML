(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace/fixSet.mli,v 1.2 2000/02/11 16:15:48 fpottier Exp $ *)

(* This module accepts a general-purpose implementation of sets, and specializes its signature by fixing the type
   of elements, as well as the ordering on elements.

   The correspondence between [t], our new type of fixed sets, and [Element.t X.t], a specific instance of the old,
   polymorphic type of sets, should not be used in most applications. However, some applications (such as the
   definition of [FixMap]) require it. *)

module Make (X : GSet.S) (Element : Map.OrderedType)
  : GSet.Fixed with type element = Element.t
                and type t = Element.t X.t

