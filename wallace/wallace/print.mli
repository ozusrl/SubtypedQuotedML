(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace/print.mli,v 1.7 2000/02/11 16:15:51 fpottier Exp $ *)

(* This is Wallace's abstract term pretty-printer. It is parameterized by two printing modules. The first one, the
   ``generic printer'', knows about the particular output format which is being used (e.g. plain text, \TeX, HTML,
   etc.), and nothing else. Several such modules are part of Wallace. The second one, the ``token printer'', also
   knows about the current output format. Furthermore, it knows about the set of tokens required by the current
   application, and is able to print them. No such module is bundled with Wallace; rather, one should be written for
   each application, since it depends on the type language.

   This module is also parameterized by a ground signature, as usual. It uses the ground signature's [Print]
   sub-module, which doesn't know about the output format (which is why abstract tokens must be used), but does know
   about the structure of terms.

   The pretty-printer provides logic for performing on-the-fly variable and term substitutions, for naming variables,
   for printing constraint graphs, and for glueing everything together. *)

(*i --------------------------------------------------------------------------------------------------------------- i*)
(* This is the interface expected of a generic printer. It must provide a set of output primitives. *)

module type GenericPrinter = sig

  (* [variable sign n] prints a type variable, given its [sign] and its unique number [n]. Numbers range up starting
     from 1. *)

  type sign

  val variable: sign -> int -> unit

  (* [box p] invokes the printing function [p] and lets it print within a box (if the notion of box makes sense for
     this printer). *)

  val box: (unit -> unit) -> unit

  (* [parentheses p] invokes the printing function [p] and lets it print within parentheses within a box (if the
     notion of box makes sense for this printer). *)

  val parentheses: (unit -> unit) -> unit

  (* [angle p] invokes the printing function [p] and lets it print within angle brackets within a box (if the
     notion of box makes sense for this printer). *)

  val angle: (unit -> unit) -> unit

  (* [comma] (resp. [colon], [semi]) prints a comma (resp. colon, semicolon), followed by a breakable space.  *)

  val comma: unit -> unit
  val colon: unit -> unit
  val semi: unit -> unit

  (* [less] prints a $\leq$ symbol, surroundered by breakable spaces. *)

  val less: unit -> unit

  (* [conditional] prints a conditional constraint. Its condition and conclusion are supplied as arguments. *)

  val conditional: (unit -> unit) -> (unit -> unit) -> unit

  (* [newline] forces an end-of-line. *)

  val newline: unit -> unit

  (* [label] prints a row label. *)

  val label: string -> unit

  (* [urow] prints a $\urow$ constructor. *)

  val urow: unit -> unit

end

(*i --------------------------------------------------------------------------------------------------------------- i*)
(* This is the interface expected of a token printer. It must provide a way of printing tokens. *)

module type TokenPrinter = sig

  (* Tokens are abstract values which stand for concrete elements such as commas, colons, arrows, etc. *)

  type token

  (* [token] prints a token. *)

  val token: token -> unit

end

(*i --------------------------------------------------------------------------------------------------------------- i*)
(* Here comes the core of the abstract pretty-printer. *)

module type S = sig

  type scheme

  (* [scheme substitute psigma sigma] prints a type scheme. The type scheme is specified by the two
     arguments [psigma] and [sigma]. Both are abstract type schemes, i.e. functions which allow iterating over the
     scheme's entry points. However, [psigma] is allowed to perform (printing) side-effects between two entry points,
     while [sigma] isn't. After applying [psigma] to display the type scheme's body, [scheme] prints its constraint
     graph.

     [substitute] is a Boolean flag, and tells whether on-the-fly variable substitution should be performed. If so,
     the type scheme must have been run through garbage collection. The pretty-printer will then transparently
     replace positive (resp. negative) variables with their unique lower (resp. upper) bound, if they have one,
     so as to make the type scheme more readable. *)

  val scheme: bool -> scheme -> scheme -> unit
    
end

module Make
    (G : Ground.Signature)
    (LeafSet : LeafSet.S)
    (CondSet : CondSet.S)
    (RowMap : RowMap.S)
    (Label : Label.S with type t = RowMap.key)
    (Core : Core.S with type 'a row_map = 'a RowMap.t
                    and type symbol = G.Symbol.t
	            and type kind = G.Kind.t
                    and type 'a preterm = 'a G.Term.t
		    and type 'a set1 = 'a LeafSet.t
		    and type 'a set2 = 'a CondSet.t)
    (GenericPrinter : GenericPrinter with type sign = Core.sign)
    (TokenPrinter : TokenPrinter with type token = G.Print.token)

: S with type scheme = Core.scheme

