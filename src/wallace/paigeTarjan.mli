(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace/paigeTarjan.mli,v 1.2 2000/02/11 16:15:50 fpottier Exp $ *)

(* This module offers a hybrid algorithm. It combines Hopcroft's algorithm for finding equivalent states in a
   deterministic finite-state automaton with Paige and Tarjan's, which is a generalization of the former to the
   non-deterministic case.

   The problem solved by this algorithm may be stated as follows: given a partition $P$ of a base set $S$, and a
   finite number of binary relations over $S$, find the coarsest refinement of $P$ which is stable with respect
   to all of these relations.

   The algorithm's theoretical complexity is $O(r^2N\log N)$, where $r$ is the number of relations involved, and
   $N$ equals $m+n$, where $m$ is the cardinality of $S$, and $n$ is the sum of all relations' cardinalities.
   In other words, when dealing with a finite-state automaton, $r$ is the alphabet's cardinality, and $N$ is
   the automaton's size, measured by counting both states and transitions.

   In the special case where a relation is functional (i.e. each point has at most one image through it), we call it a
   function, and are able to deal with it more efficiently. (The theoretical difference is a constant factor, which in
   practice approximately equals $2$.) In particular, in the special case where all relations are functions (which
   occurs when minimizing a deterministic finite-state automaton), the algorithm is exactly Hopcroft's---no
   penalty is paid for the added power of Paige and Tarjan's algorithm. *)

module Make (P : sig

  (* The module is parameterized over the abstract data type of points, which represent elements of the set $S$. *)

  type point

  (* The algorithm needs to associate an internal data structure with each point. The implementation of the
     association mechanism is left to the client, rather than performed internally. The reason is that the client can
     easily implement a very efficient association mechanism, by adding a mutable field to the concrete data structure
     used to represent points.

     Unfortunately, this design causes problems at the level of types. Indeed, it makes the client's module mutually
     recursive with this module, at least at the level of types: the client's concrete data structure must contain
     a field whose type is defined by this module. Because mutually recursive modules are not (yet?) allowed by
     O'Caml, we have to break its typing discipline. The specification below requires the client to provide us with
     a field of an arbitrary type, but we shall use it internally to store our own information, without respect for
     its type. *)

  type dummy

  val store: point -> dummy -> unit
  val get: point -> dummy

  (* When the algorithm is done, it needs a way of telling the client which points are equivalent (i.e. belong to a
     single class in the final, refined partition). It does so by choosing a representative in each equivalence class
     and telling the client about each point's representative. *)

  val represents: point -> point -> unit

end) : sig

  type point = P.point

  (* Here come the functions which allow setting up and running the algorithm. There are several steps, which must
     be performed in order. *)

  (* First, the client must tell how many functions and relations, respectively, are present. Note that it is correct
     to describe all relations as such, even if some of them are in fact functional. The distinction between functions
     and relations is a mere matter of efficiency: if the client is able to determine that some relations are
     functional, describing them as functions allows the hybrid algorithm to deal with them more efficiently, thus
     saving time. *)

  val set_counts: int -> int -> unit

  (* Second, the client must define the set $S$, together with the initial partition $P$, by enumerating points. The
     call [add_point nb p] defines a new point [p]. The flag [nb] tells whether this should be the beginning of a new
     block of the partition $P$. (This means that the order of calls to [add_point] is significant.) When [add_point]
     is called for the first time, a new block is always created, regardless of [nb]'s value. *)

  val add_point: bool -> point -> unit

  (* Third, the client must define functions and relations by enumerating relationships. The call
     [add_functional_link i p1 p2] states that [p2] is the image of [p1] through the function of index [i]. The call
     [add_relational_link i p1 p2] states that [p1] and [p2] are related through the relation of index [i]. Functions
     (resp. relations) are indexed from $0$ (inclusive) up to [rf] (resp. [rr]) (exclusive), where [rf] and [rr] are
     the values previously passed to [set_counts]. *)

  val add_functional_link: int -> point -> point -> unit
  val add_relational_link: int -> point -> point -> unit

  (* Lastly, the client can run the refinement algorithm. Upon completion, the algorithm shall call [P.represents]
     repeatedly so as to define each point's representative in the final (refined) partition. *)

  val refine: unit -> unit

end
