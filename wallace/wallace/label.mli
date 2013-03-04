(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace/label.mli,v 1.3 2000/02/11 16:15:49 fpottier Exp $ *)

(* A type parser expects row labels to be strings. However, internally, it is possible to give them some more
   efficient representation, such as integers. Some module must allow conversions between the two formats. Such
   a module's signature is defined here. *)

module type S = sig

  (* The internal type of labels. *)

  type t

  (* [get] expects a string and returns the corresponding label, in internal form. *)

  val get: string -> t

  (* [print] expects a label and returns the corresponding string (i.e. the string which was passed to [get] when
     the label was created). *)

  val print: t -> string

end

(* Let us now give a trivial implementation of the above signature, where both operations are the identity. It has the
   advantage of simplicity, but may slow down operations, since it forces the type inference engine to perform many
   string comparisons while dealing with rows. *)

module Simple : S with type t = string

(* Here is a more interesting implementation of signature [S], where labels are integers. *)

module Integer : sig

  (* [get] works as specified above, except it raises [Clash] if two strings map to the same internal label. *)

  exception Clash of string * string

  include (S with type t = int)

end

