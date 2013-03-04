(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace/core.mli,v 1.20 2000/02/11 16:15:48 fpottier Exp $ *)

(* This module defines the library's fundamental data structures, used to represent type variables and subtyping
   constraints. *)

module type S = sig

  type symbol
  type kind
  type 'a expression
  type 'a coercion
  type 'a preterm
  type 'a set1
  type 'a set2
  type 'a set3

  (* To implement rows, we need maps from labels to types. The type of row labels is kept abstract in this module;
     the polymorphic type ['a row_map] is assumed to provide maps from labels to some type ['a]. We also need to
     represent ``spans'', which are sets of labels; we assume another abstract type, [span].

     A row consists of a set of labelled entries (i.e. a finite map from labels to content), plus one piece of
     remainder data. This definition of rows is polymorphic in the content's type. *)

  type 'a row_map
  type span

  type 'a row = {
      entries: 'a row_map;
      remainder: 'a
    } 

  (* A [variable] is a structure which carries constraints. *)

  type variable = {

      (* Each variable is assumed to appear within at most one constraint set. This allows us to store information
	 about the variable's lower and upper bounds within the variable itself. A variable's lower (resp. upper)
	 bounds consist of a single type term, together with a set of variables. *)

      mutable lo: term;
      mutable loset: leaf_set;
      mutable hiset: leaf_set;
      mutable hi: term;

      (* Additionally, each variable carries a list of conditional constraints, to be triggered if the variable's
	 lower bound outgrows a certain value. *)

      mutable conditionals: conditional_set

    } 

  (* A ``leaf'' is either a variable or a row. Leaves are our central data structure: they constitute the nodes in the
     constraint graph. Edges coming out of a given leaf are either subtyping constraints, if the leaf is a variable,
     or equality edges, if the leaf is a row. *)

  and leaf_link =
    | VarLink of variable
    | RowLink of leaf row

  and leaf = {

      (* Each leaf carries a unique integer stamp. This allows defining a total ordering on leaves, a property
	 required by certain data structures, mostly sets of leaves. *)

      stamp: int;

      (* A field allows marking leaves while traversing constraints. It should not be used directly; appropriate
	 operations are provided by the [Traverse] sub-module. *)

      mutable traversal: int;

      (* This field is used to speed up various substitution and copy operations. *)

      mutable representative: leaf;

      (* Because every row variable may be expanded into a row, a mutable field is used to keep track of the
	 relationship between the leaf and the entity it stands for. *)

      mutable link: leaf_link;

      (* According to the theory of rows, each leaf carries a sort, which tells whether it represents a regular type
	 or a row type. In the latter case, the sort also tells which labels \emph{cannot} be extracted out of this
	 row.

	 In practice, all closure operations preserve well-sortedness, so one may think that it is not necessary to
	 keep track of sorts at run-time. Does this intuition hold? Here's the answer:
	 \begin{itemize}
	 \item We choose not to represent the $\urow$ constructor within our data structures. Instead, its presence
	       shall be implicitly detected whenever a row mismatch is found. So, we must explicitly keep track of
               the distinction between regular variables and row variables.
         \item Minimization, in its default formulation, does not preserve well-sortedness. (For instance,
               unconstrained variables of different sorts will be merged.)
	 \end{itemize}
	 For these two reasons, we do keep track of sorts. A leaf's sort is either ``regular'', or ``row''.
	 In the latter case, the sort also includes a set of non-admissible labels, which we call the leaf's
	 ``span''. *)

      sort: sort;

      (* This field is used by the closure algorithm. Given a fresh leaf, it allows determining, in constant
	 time, which least upper bound (resp. greatest lower bound) expression it stands for.

	 Notice that we only store a set of original leaves, not the operator (join or meet) that was applied
	 to them. This is because it shall be always be known from the context. (More precisely, a fresh leaf
	 created by a LUB computation shall never be involved in a subsequent GLB computation, and vice-versa.)

	 This field is [None] when outside of the closure algorithm. While the algorithm is running, it is [None] in
	 an original leaf, and points to a set of original leaves in a fresh leaf. *)

      (* TEMPORARY could re-use representative (with some magic) field and save space (one field in this structure,
	 plus no need to use an option, just compare (==) with the leaf itself as usual). Dirty, though. *)

      mutable originals: original_set option;

      (* Each leaf may carry a sign (neutral, positive, negative or both). This piece of information is not
	 necessarily meaningful at all times; it is computed by the garbage collection algorithm. It would be
	 possible to store it only in variables, rather than in every leaf, but this is slightly simpler. *)

      mutable sign: sign

    } 

  and sort =
    | Regular
    | Row of span

  and sign = 
    | Neutral
    | Positive
    | Negative
    | Bipolar

  and conditional = symbol * leaf * leaf

  and term = leaf preterm
  and leaf_set = leaf set1
  and conditional_set = conditional set2
  and original_set = leaf set3

  (* A total ordering on leaves, used when storing leaves in sets, association maps, or other similar structures. *)

  val clv: leaf -> leaf -> int

  (* The library has an abstract notion of type scheme. A type scheme is simply a set of entry points, some of which
     are positive, some of which are negative. Each of them is occupied by a leaf. These leaves are (implicitly)
     related by constraints, hence a notion of constrained type scheme.

     So as to avoid committing to any concrete data structure for type schemes, we define a type scheme as a
     function which allows iterating over the scheme's entry points. When it is applied to a function [f],
     it invokes [f sign lf] for each entry point [lf], where [sign] is the entry point's sign.

     Sometimes, Wallace also needs to copy type schemes, rather than just iterate. So, it sometimes requires a ``type
     scheme mapper'', i.e. a function which, when itself passed a function capable of copying variables, creates a
     copy of the original type scheme. In other words, this function should apply its argument to each of the type
     scheme's entry points, and combine the results into a new type scheme. *)

  type scheme =
      (bool -> leaf -> unit) -> unit

  type 'a scheme_mapper =
      (leaf -> leaf) -> 'a

  (* The following set of operations allows creating fresh variables. *)

  module Fresh : sig

    (* [lo s t] (resp. [hi s t]) creates a fresh variable leaf of sort [s] and makes [t] its lower (resp. upper)
       constructed bound. *)

    val lo: sort -> term -> leaf
    val hi: sort -> term -> leaf

    (* [pair s k] creates a pair of fresh variable leaves of sort [s] and kind [k], and links them with a subtyping
       constraint. *)

    val pair: sort -> kind -> leaf * leaf

  end

  module Traverse : sig

    (* The following operation allows applying a specified action to each leaf which appears within a given
       constrained type scheme.

       The action function is passed each leaf \emph{before} the constraints (or link) bearing upon it are
       examined. Thus, if the user modifies this information, the graph traversal \emph{will} be influenced. *)

    val scheme: (leaf -> unit) -> scheme -> unit

  end

  module Row : sig

    (* [link lf] returns the contents of [lf]'s [link] field, with one additional guarantee: if the function returns
       [RowLink row], then [row] is guaranteed to be ``clean'', i.e. its remainder leaf is guaranteed to be a
       variable. The function internally performs path compression, so as to speed up further accesses. *)

    val link: leaf -> leaf_link

    (* [iter action row] applies the specified action to all elements of a row, including its remainder. The action
       function does not have access to each element's label. *)

    val iter: ('a -> unit) -> 'a row -> unit

  end

  (* The following operation allows taking a fresh copy of a type scheme, i.e. a new type scheme, which is identical,
     except each variable has been replaced with a fresh copy. This reflects the fact that, in our current theory,
     every variable which appears in a type scheme must be understood as universally quantified. *)

  module Copy : sig

    val scheme: 'a scheme_mapper -> 'a

  end

  (* The following set of operations allow (incrementally) computing the closure of a constraint graph. *)

  module Closure : sig

    (* The exception [Inconsistent] is raised when the closure algorithm discovers an inconsistency. *)

    exception Inconsistent

    (* Calling [link lf1 lf2] adds a constraint from [lf1] to [lf2] in the constraint graph, and performs suitable
       closure operations. It raises [Inconsistent] if this operation makes the constraints unsatisfiable. [lf1] and
       [lf2] must have the same kind. *)

    val link: leaf -> leaf -> unit

    (* Calling [lo term lf] makes [term] a lower bound of [lf], and performs suitable closure operations. It raises
       [Inconsistent] if this operation makes the constraints unsatisfiable. [term] and [lf] must have the same
       kind. [lf] must be a variable (i.e. not a row). [hi lf term] behaves symmetrically. *)

    val lo: term -> leaf -> unit
    val hi: leaf -> term -> unit

    (* [conditional s lf0 lf1 lf2] creates a new conditional constraint $\cc{s}{[lf0]}{[lf1]}{[lf2]}$. It raises
       [Inconsistent] if this operation makes the constraints unsatisfiable. [lf0], [lf1] and [lf2] must have the same
       span (i.e. they may be regular or row variables, but all row variables involved must have the same span). [s]
       and [lf0] must have the same kind; [lf1] and [lf2] must have the same kind. *)

    val conditional: symbol -> leaf -> leaf -> leaf -> unit

    (* [meet] (resp. [join]) returns a fresh leaf which stands for the meet (resp. join) of its two arguments.
       It is simply a shortcut for two appropriate calls to [link]. Its two arguments must have the same kind. *)

    val meet: leaf -> leaf -> leaf
    val join: leaf -> leaf -> leaf

  end

  (* The following function performs in-place garbage collection on the specified type scheme. As a side-effect, the
     scheme's variables are annotated with correct signs. *)

  module Garbage : sig

    val collect: scheme -> unit

  end

  (* The following function performs minimization of the specified type scheme. That is, it classifies leaves into
     equivalence classes, and chooses a representative in each class. It then proceeds to replace each leaf with its
     representative within the type scheme. *)

  module Minimize : sig

    val minimize: scheme -> 'a scheme_mapper -> 'a

  end

  (* The following module allows translating type scheme expressions into actual (internal) type schemes. It
     accepts a type scheme expression, expressed in the form of
     \begin{itemize}
     \item a function which allows iterating over the scheme's entries (where each entry consists of a type
           expression), and provides their expected kind;
     \item a list of constraints, which are pairs of type expressions.
     \end{itemize}
     The function returns a translation function, which should be applied to the type scheme's entry points
     to produce the final (internal) type scheme. In other words, the translation function should be passed
     to the [smap] function over concrete type schemes.

     The [expression] function or the translation function will raise [Kind.Inconsistency] if the type scheme
     expression is ill-kinded. The latter will raise [Kind.UnderSpecified] if some type variable's kind cannot be
     determined. The exception carries the name of one such type variable. This may occur if some constraints are
     unreachable from the type scheme's entry points.

     Each of the scheme's entries is implicitly assumed to have sort [Regular], which is why the [entries]
     function is not required to provide each entry's sort.

     The [expression] function or the translation function will raise [Sort.Inconsistency] if the type scheme
     expression is ill-kinded. The latter will raise [Sort.UnderSpecified] if some
     expression's sort cannot be determined. This may occur if some constraints are unreachable from the type scheme's
     entry points.

     During translation, expressions are internally annotated with information of type [info], which remains
     abstract. *)

  module Translate : sig
    
    type info

    type entries =
	(kind -> info expression -> unit) -> unit

    and constraints =
	(info coercion) list

    module Kind : sig

      exception Inconsistency
      exception UnderSpecified of string

    end

    module Sort : sig

      exception Inconsistency
      exception UnderSpecified

    end

    type translation_function =
      |	TransFun of (bool -> info expression -> leaf)

    val expression: entries -> constraints -> translation_function

  end

end

(* This module is parameterized over:
   \begin{itemize}
   \item a ground signature, [G];
   \item an implementation of sets, for use with leaves, [LeafSet];
   \item an implementation of sets, for use with conditional constraints, [CondSet];
   \item an implementation of maps whose keys are sets, for use by the closure algorithm, [LeafSetMap];
   \item an implementation of maps whose keys are row labels, for use in rows, [RowMap].
   \item a way of turning strings into row labels, provided by module [Label].
   \end{itemize} *)

module Make 
    (G : Ground.Signature)
    (LeafSet : LeafSet.S)
    (CondSet : CondSet.S)
    (LeafSetMap : LeafSetMap.S)
    (RowMap : RowMap.S)
    (Label : Label.S with type t = RowMap.key)
    :
    S with type 'a row_map = 'a RowMap.t
       and type symbol = G.Symbol.t
       and type kind = G.Kind.t
       and type 'a expression = 'a G.Abstract.expression
       and type 'a coercion = 'a G.Abstract.coercion
       and type 'a preterm = 'a G.Term.t
       and type 'a set1 = 'a LeafSet.t
       and type 'a set2 = 'a CondSet.t
       and type 'a set3 = 'a LeafSetMap.Set.t

