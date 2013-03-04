(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/gromit/generator.mli,v 1.3 2000/02/11 16:15:35 fpottier Exp $ *)

open Signature

(* This is Gromit's code generator. Given the original file name, as well as a [Signature.signature] structure
   which describes its contents, the code generator produces Objective Caml file(s) containing an implementation
   of the signature. *)

val generate: string -> signature -> unit

