(** Copyright 2023-2024, Vladislav Aliev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type fresh = int

module R : sig
  type 'a t = fresh -> fresh * ('a, Typing.error) result

  val ( >>= )
    :  ('a -> 'b * ('c, 'd) result)
    -> ('c -> 'b -> 'b * ('e, 'd) result)
    -> 'a
    -> 'b * ('e, 'd) result

  val ( >>| ) : ('a -> 'b * ('c, 'd) result) -> ('c -> 'e) -> 'a -> 'b * ('e, 'd) result
  val fail : 'a -> 'b -> 'b * ('c, 'a) result
  val return : 'a -> 'b -> 'b * ('a, 'c) result

  val bind
    :  ('a -> 'b * ('c, 'd) result)
    -> f:('c -> 'b -> 'b * ('e, 'd) result)
    -> 'a
    -> 'b * ('e, 'd) result

  module Syntax : sig
    val ( let* )
      :  ('a -> 'b * ('c, 'd) result)
      -> ('c -> 'b -> 'b * ('e, 'd) result)
      -> 'a
      -> 'b * ('e, 'd) result
  end

  val fresh : fresh -> fresh * (fresh, 'a) result
  val run : (fresh -> 'a * 'b) -> 'b

  module RMap : sig
    val fold_left
      :  ('a, 'b, 'c) Base.Map.t
      -> init:('d -> 'e * ('f, 'g) result)
      -> f:('a -> 'b -> 'f -> 'e -> 'e * ('f, 'g) result)
      -> 'd
      -> 'e * ('f, 'g) result
  end

  type fresh = int
end

module Type : sig
  type t = Typing.typ

  val occurs_in : fresh -> Typing.typ -> bool
  val free_vars : Typing.typ -> (fresh, Base.Int.comparator_witness) Base.Set.t
end

module Subst : sig
  type t = (fresh, Typing.typ, Base.Int.comparator_witness) Base.Map.t

  val empty : (fresh, 'a, Base.Int.comparator_witness) Base.Map.t

  val mapping
    :  fresh
    -> Typing.typ
    -> 'a
    -> 'a * (fresh * Typing.typ, [> `OccursCheckFailed ]) result

  val singleton
    :  fresh
    -> Typing.typ
    -> 'a
    -> 'a
       * ( (fresh, Typing.typ, Base.Int.comparator_witness) Base.Map.t
           , [> `OccursCheckFailed ] )
           result

  val find : ('a, 'b, 'c) Base.Map.t -> 'a -> 'b option
  val remove : ('a, 'b, 'c) Base.Map.t -> 'a -> ('a, 'b, 'c) Base.Map.t
  val apply : (fresh, Typing.typ, 'a) Base.Map.t -> Typing.typ -> Typing.typ

  val unify
    :  Typing.typ
    -> Typing.typ
    -> 'a
    -> 'a
       * ( (fresh, Typing.typ, Base.Int.comparator_witness) Base.Map.t
           , [> `OccursCheckFailed | `UnificationFailed of Typing.typ * Typing.typ ] )
           result

  val extend
    :  fresh
    -> Typing.typ
    -> (fresh, Typing.typ, Base.Int.comparator_witness) Base.Map.t
    -> 'a
    -> 'a
       * ( (fresh, Typing.typ, Base.Int.comparator_witness) Base.Map.t
           , [> `OccursCheckFailed | `UnificationFailed of Typing.typ * Typing.typ ] )
           result

  val compose
    :  (fresh, Typing.typ, Base.Int.comparator_witness) Base.Map.t
    -> (fresh, Typing.typ, Base.Int.comparator_witness) Base.Map.t
    -> 'a
    -> 'a
       * ( (fresh, Typing.typ, Base.Int.comparator_witness) Base.Map.t
           , [> `OccursCheckFailed | `UnificationFailed of Typing.typ * Typing.typ ] )
           result

  val compose_all
    :  (fresh, Typing.typ, Base.Int.comparator_witness) Base.Map.t list
    -> 'a
    -> 'a
       * ( (fresh, Typing.typ, Base.Int.comparator_witness) Base.Map.t
           , [> `OccursCheckFailed | `UnificationFailed of Typing.typ * Typing.typ ] )
           result
end

type scheme = (fresh, Base.Int.comparator_witness) Base.Set.t * Typing.typ

module Scheme : sig
  type t = scheme

  val empty : (fresh, Base.Int.comparator_witness) Base.Set.t
  val occurs_in : fresh -> (fresh, 'a) Base.Set.t * Typing.typ -> bool

  val free_vars
    :  (fresh, Base.Int.comparator_witness) Base.Set.t * Typing.typ
    -> (fresh, Base.Int.comparator_witness) Base.Set.t

  val apply
    :  (fresh, Typing.typ, 'a) Base.Map.t
    -> (fresh, 'b) Base.Set.t * Typing.typ
    -> (fresh, 'b) Base.Set.t * Typing.typ
end

module TypeEnv : sig
  type t = (string, scheme, Base.String.comparator_witness) Base.Map.t

  val extend : ('a, 'b, 'c) Base.Map.t -> 'a -> 'b -> ('a, 'b, 'c) Base.Map.t
  val remove : ('a, 'b, 'c) Base.Map.t -> 'a -> ('a, 'b, 'c) Base.Map.t
  val empty : (string, 'a, Base.String.comparator_witness) Base.Map.t
  val free_vars : t -> (fresh, Base.Int.comparator_witness) Base.Set.t

  val apply
    :  (fresh, Typing.typ, 'a) Base.Map.t
    -> ('b, (fresh, 'c) Base.Set.t * Typing.typ, 'd) Base.Map.t
    -> ('b, (fresh, 'c) Base.Set.t * Typing.typ, 'd) Base.Map.t

  val find : 'a -> ('a, 'b, 'c) Base.Map.t -> 'b option
end

val unify
  :  Typing.typ
  -> Typing.typ
  -> 'a
  -> 'a
     * ( (int, Typing.typ, Base.Int.comparator_witness) Base.Map.t
         , [> `OccursCheckFailed | `UnificationFailed of Typing.typ * Typing.typ ] )
         result

val fresh_var : int -> int * (Typing.typ, 'a) result

val instantiate
  :  (int, 'a) Base.Set.t * Typing.typ
  -> int
  -> int * (Typing.typ, [> `OccursCheckFailed ]) result

val generalize
  :  TypeEnv.t
  -> Typing.typ
  -> (int, Base.Int.comparator_witness) Base.Set.t * Typing.typ

val generalize_rec
  :  (string, scheme, Base.String.comparator_witness) Base.Map.t
  -> Typing.typ
  -> string
  -> (int, Base.Int.comparator_witness) Base.Set.t * Typing.typ

val lookup_env
  :  'a
  -> ('a, (int, 'b) Base.Set.t * Typing.typ, 'c) Base.Map.t
  -> int
  -> int
     * ( (int, 'd, Base.Int.comparator_witness) Base.Map.t * Typing.typ
         , [> `OccursCheckFailed | `UnknownVariable of 'a ] )
         result

val infer_pat
  :  (string, (int, Base.Int.comparator_witness) Base.Set.t * Typing.typ, 'a) Base.Map.t
  -> Ast.pattern
  -> int
  -> int
     * ( ( string
           , (int, Base.Int.comparator_witness) Base.Set.t * Typing.typ
           , 'a )
           Base.Map.t
         * Typing.typ
         , [> `OccursCheckFailed | `UnificationFailed of Typing.typ * Typing.typ ] )
         result

val infer_expr
  :  ( string
       , (int, Base.Int.comparator_witness) Base.Set.t * Typing.typ
       , Base.String.comparator_witness )
       Base.Map.t
  -> Ast.expr
  -> int
  -> int
     * ( (int, Typing.typ, Base.Int.comparator_witness) Base.Map.t * Typing.typ
         , [> `NotImplemented
           | `OccursCheckFailed
           | `UnificationFailed of Typing.typ * Typing.typ
           | `UnknownVariable of string
           ] )
         result

val infer_decl
  :  (string, scheme, Base.String.comparator_witness) Base.Map.t
  -> Ast.decl
  -> int
  -> int
     * ( (string, scheme, Base.String.comparator_witness) Base.Map.t
         , [> `NotImplemented
           | `OccursCheckFailed
           | `UnificationFailed of Typing.typ * Typing.typ
           | `UnknownVariable of string
           ] )
         result

val infer_program
  :  Ast.program
  -> int
  -> int
     * ( (string, scheme, Base.String.comparator_witness) Base.Map.t
         , [> `NotImplemented
           | `OccursCheckFailed
           | `UnificationFailed of Typing.typ * Typing.typ
           | `UnknownVariable of string
           ] )
         result

val run_inference
  :  Ast.program
  -> ( (string, scheme, Base.String.comparator_witness) Base.Map.t
       , [> `NotImplemented
         | `OccursCheckFailed
         | `UnificationFailed of Typing.typ * Typing.typ
         | `UnknownVariable of string
         ] )
       result

val print_env
  :  ( (string, 'a * Typing.typ, 'b) Base.Map.t
       , [< `MatchFailure
         | `NotImplemented
         | `OccursCheckFailed
         | `UnificationFailed of Typing.typ * Typing.typ
         | `UnknownVariable of string
         ] )
       result
  -> unit
