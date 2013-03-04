(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/toy/engine.mli,v 1.13 2000/02/11 16:16:33 fpottier Exp $ *)

(* This module defines a typechecker for a simple $\lambda$-calculus. *)

module type S = sig

  (* At the core of the engine is Wallace. *)

  module Core : Core.S

  (* Type schemes and environments are made abstract in the module's external interface. *)

  type scheme
  type environment

  val builtin: environment

  (* Printing type schemes. *)

  module Print : sig

    val text : bool -> scheme -> unit

  end

  (* This function simplifies a type scheme. It is destructive, i.e. its argument must no longer be used afterwards.
     TEMPORARY could/should be made internal *)

  val simplify: scheme -> scheme

  (* The type inference engine. *)

  exception Failure of string

  val infer: environment -> InternalSyntax.expression -> scheme

end

(* The module is parameterized by an implementation of environments. It is also parameterized by an implementation
   of contexts, which are maps from internal identifiers to type variables. *)

module Make
  (Env : Env.S)
  (IdMap : Context.S with type key = Env.identifier)
: S

