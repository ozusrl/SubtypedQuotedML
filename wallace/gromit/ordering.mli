(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/gromit/ordering.mli,v 1.2 2000/02/11 16:15:36 fpottier Exp $ *)

(* This module offers a few basic operations concerning finite partial orders, henceforth referred to as
   \emph{orderings}. *)

(*i-----------------------------------------------------------------------------------------------------------------i*)
(*s \mysection{Orderings} *)

(* A binary relation over a set of $n$ elements is represented as a $n\times n$ square Boolean matrix. Elements are
   numbered from $0$ to $n-1$. An ordering is a reflexive, transitive, antisymmetric relation. *)

type ordering = {
    ordering_n : int;
    ordering_matrix : bool array array
  } 

(* The function [ordering] expects an arbitrary binary relation, and computes the smallest ordering containing it, if
   there is one.  The computation is performed in place. An exception of the form [Cycle(i, j)] is raised if the
   relation contains a cycle. Then, [i] and [j] are two distinct, unspecified elements of such a cycle. *)

exception Cycle of int * int

val ordering: ordering -> unit

(*i-----------------------------------------------------------------------------------------------------------------i*)
(*s \mysection{Lattices} *)

(* A lattice is an ordering which verifies a few additional properties. It must have a least element $\bot$ and a
   greatest element $\top$. Furthermore, any two elements $i$ and $j$ must have a least upper bound $i\sqcup j$ and
   a greatest lower bound $i\sqcap j$. *)

type lattice = {
    lattice_n : int;
    lattice_matrix : bool array array;
    lattice_bot : int;
    lattice_top : int;
    lattice_lub : int array array;
    lattice_glb : int array array
  } 

(* The function [lattice] expects an ordering, makes sure it is a lattice, and returns an appropriate [lattice]
   structure describing it. The following exceptions may be raised if the ordering isn't a lattice:
   \begin{itemize}
   \item [Empty]: the ordering has no elements;
   \item [NoLUB(i, j)]: $i\sqcup j$ is undefined;
   \item [NoGLB(i, j)]: $i\sqcap j$ is undefined.
   \end{itemize} *)

exception Empty
exception NoLUB of int * int
exception NoGLB of int * int

val lattice: ordering -> lattice

