(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace/standard.mli,v 1.3 2000/02/11 16:15:52 fpottier Exp $ *)

(* This module implements miscellaneous common operations, which may be considered as missing from the language's
   standard library, hence its name. *)

(* [do_option (Some value) action] invokes [action value]. [do_option None action] has no effect. *)

val do_option: 'a option -> ('a -> unit) -> unit

(* [array_of_measured_list n list] turns [list] into an array. [list] must have length [n]. *)

val array_of_measured_list: int -> 'a list -> 'a array

(* [filter_map] expects a ``constructive predicate'' [p] and a list [l]. A ``constructive predicate'' is a function
   which, given an element [x], returns either another [x'], or nothing. [filter_map p l] returns the list obtained
   by applying [p] to each element of [l], and gathering any elements returned by [p]. [filter_map] is thus a
   combination of [filter] and [map]. *)

val filter_map: ('a -> 'a option) -> 'a list -> 'a list

