(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/toy/simpleEnv.mli,v 1.3 2000/02/11 16:16:34 fpottier Exp $ *)

(* This module provides a straightforward implementation of environments. It is parameterized by a general-purpose
   implementation of maps. One may simply use unordered lists, as is usually done, or provide a more evolved data
   structure. *)

module Make (Map : GMap.Additive) : Env.S with type identifier = int

