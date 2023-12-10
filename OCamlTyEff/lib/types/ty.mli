open! Base

type t =
  | Ty_var of Var.t  (** A type variable such as ['a] *)
  | Ty_arr of t * t  (** [Ty_arr(T1, T2)] represents [T1 -> T2] *)
  | Ty_tuple of t list
      (** [Ty_tuple([T1 ; ... ; Tn])] represents [T1 * ... * Tn].
          Invariant: [n >= 2].
        *)
  | Ty_con of Ast.Ident.t * t list
      (** [Ty_con(ident, l)] represents:
           - [tconstr]               when [l=[]],
           - [T tconstr]             when [l=[T]],
           - [(T1, ..., Tn) tconstr] when [l=[T1 ; ... ; Tn]].
        *)

val pp : Format.formatter -> t -> unit

val equal : t -> t -> bool

val compare : t -> t -> int

val sexp_of_t : t -> Sexp.t

val unit : t
(** Unit type *)

val int : t
(** Int type *)

val bool : t
(** Bool type *)

val char : t
(** Char type *)

val string : t
(** String type *)

val vars : t -> VarSet.t
(** Type variables occuring in a type *)