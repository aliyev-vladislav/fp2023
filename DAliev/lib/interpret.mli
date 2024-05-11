(** Copyright 2023-2024, Vladislav Aliev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module type MONAD = sig
  type ('a, 'e) t

  val return : 'a -> ('a, 'e) t
  val fail : 'e -> ('a, 'e) t
  val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
end

module MONAD_RESULT : sig
  type ('ok, 'err) t = ('ok, 'err) result =
    | Ok of 'ok
    | Error of 'err

  val t_of_sexp
    :  (Sexplib0__.Sexp.t -> 'a)
    -> (Sexplib0__.Sexp.t -> 'b)
    -> Sexplib0__.Sexp.t
    -> ('a, 'b) t

  val sexp_of_t
    :  ('a -> Sexplib0__.Sexp.t)
    -> ('b -> Sexplib0__.Sexp.t)
    -> ('a, 'b) t
    -> Sexplib0__.Sexp.t

  val t_sexp_grammar
    :  'ok Sexplib0.Sexp_grammar.t
    -> 'err Sexplib0.Sexp_grammar.t
    -> ('ok, 'err) t Sexplib0.Sexp_grammar.t

  val compare
    :  'a Base.Exported_for_specific_uses.Ppx_compare_lib.compare
    -> 'b Base.Exported_for_specific_uses.Ppx_compare_lib.compare
    -> ('a, 'b) t Base.Exported_for_specific_uses.Ppx_compare_lib.compare

  val equal
    :  'a Base.Exported_for_specific_uses.Ppx_compare_lib.equal
    -> 'b Base.Exported_for_specific_uses.Ppx_compare_lib.equal
    -> ('a, 'b) t Base.Exported_for_specific_uses.Ppx_compare_lib.equal

  val globalize : ('ok -> 'ok) -> ('err -> 'err) -> ('ok, 'err) t -> ('ok, 'err) t

  val hash_fold_t
    :  'a Base.Exported_for_specific_uses.Ppx_hash_lib.hash_fold
    -> 'b Base.Exported_for_specific_uses.Ppx_hash_lib.hash_fold
    -> ('a, 'b) t Base.Exported_for_specific_uses.Ppx_hash_lib.hash_fold

  val ( >>= ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
  val ( >>| ) : ('a, 'e) t -> ('a -> 'b) -> ('b, 'e) t

  module Let_syntax = Base.Result.Let_syntax
  module Monad_infix = Base.Result.Monad_infix

  val bind : ('a, 'e) t -> f:('a -> ('b, 'e) t) -> ('b, 'e) t
  val return : 'a -> ('a, 'b) t
  val join : (('a, 'e) t, 'e) t -> ('a, 'e) t
  val ignore_m : ('a, 'e) t -> (unit, 'e) t
  val all : ('a, 'e) t list -> ('a list, 'e) t
  val all_unit : (unit, 'e) t list -> (unit, 'e) t

  module Error = Base.Result.Error

  val invariant
    :  'a Base__Invariant_intf.inv
    -> 'b Base__Invariant_intf.inv
    -> ('a, 'b) t Base__Invariant_intf.inv

  val fail : 'err -> ('a, 'err) t
  val failf : ('a, unit, string, ('b, string) t) format4 -> 'a
  val is_ok : ('a, 'b) t -> bool
  val is_error : ('a, 'b) t -> bool
  val ok : ('ok, 'a) t -> 'ok option
  val ok_exn : ('ok, exn) t -> 'ok
  val ok_or_failwith : ('ok, string) t -> 'ok
  val error : ('a, 'err) t -> 'err option
  val of_option : 'ok option -> error:'err -> ('ok, 'err) t
  val iter : ('ok, 'a) t -> f:('ok -> unit) -> unit
  val iter_error : ('a, 'err) t -> f:('err -> unit) -> unit
  val map : ('ok, 'err) t -> f:('ok -> 'c) -> ('c, 'err) t
  val map_error : ('ok, 'err) t -> f:('err -> 'c) -> ('ok, 'c) t

  val combine
    :  ('ok1, 'err) t
    -> ('ok2, 'err) t
    -> ok:('ok1 -> 'ok2 -> 'ok3)
    -> err:('err -> 'err -> 'err)
    -> ('ok3, 'err) t

  val combine_errors : ('ok, 'err) t list -> ('ok list, 'err list) t
  val combine_errors_unit : (unit, 'err) t list -> (unit, 'err list) t
  val to_either : ('ok, 'err) t -> ('ok, 'err) Base__Either0.t
  val of_either : ('ok, 'err) Base__Either0.t -> ('ok, 'err) t
  val ok_fst : ('ok, 'err) t -> ('ok, 'err) Base__Either0.t
  val ok_if_true : bool -> error:'err -> (unit, 'err) t
  val try_with : (unit -> 'a) -> ('a, exn) t

  module Export = Base.Result.Export

  val ( let* ) : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t
end

type value =
  | VUnit
  | VBool of bool
  | VString of string
  | VInt of int
  | VTuple of value list
  | VList of value list
  | VFun of Ast.pattern * Ast.rec_flag * Ast.expr * envr

and envr = (string, value, Base.String.comparator_witness) Base.Map.t

val pp_value : Format.formatter -> value -> unit

type error =
  [ `DivisionByZero
  | `InvalidType of value
  | `MatchFailure
  | `NotImplemented
  | `UnknownVariable of string
  ]

val pp_error
  :  Format.formatter
  -> [< `DivisionByZero
     | `InvalidType of value
     | `MatchFailure
     | `NotImplemented
     | `UnknownVariable of string
     ]
  -> unit

module Env : functor (M : MONAD) -> sig
  val empty : (string, 'a, Base.String.comparator_witness) Base.Map.t
  val extend : ('a, 'b, 'c) Base.Map.t -> 'a -> 'b -> ('a, 'b, 'c) Base.Map.t
  val find : ('a, 'b, 'c) Base.Map.t -> 'a -> ('b, [> `UnknownVariable of 'a ]) M.t
end

module Interpret : functor (M : MONAD) -> sig
  val eval_bop
    :  Ast.bop * value * value
    -> (value, [> `DivisionByZero | `InvalidType of value ]) M.t

  val eval_pattern
    :  (string, value, 'a) Base.Map.t
    -> Ast.pattern * value
    -> (string, value, 'a) Base.Map.t option

  val eval_const : Ast.const -> (value, 'a) M.t

  val seek
    :  (string, value, 'a) Base.Map.t
    -> string
    -> (value, [> `UnknownVariable of string ]) M.t

  val eval_expr
    :  (string, value, Base.String.comparator_witness) Base.Map.t
    -> Ast.expr
    -> ( value
         , [> `DivisionByZero
           | `InvalidType of value
           | `MatchFailure
           | `NotImplemented
           | `UnknownVariable of string
           ] )
         M.t

  val eval_decl
    :  (string, value, Base.String.comparator_witness) Base.Map.t
    -> Ast.decl
    -> ( (string, value, Base.String.comparator_witness) Base.Map.t
         , [> `DivisionByZero
           | `InvalidType of value
           | `MatchFailure
           | `NotImplemented
           | `UnknownVariable of string
           ] )
         M.t

  val eval_program
    :  Ast.program
    -> ( (string, value, Base.String.comparator_witness) Base.Map.t
         , [> `DivisionByZero
           | `InvalidType of value
           | `MatchFailure
           | `NotImplemented
           | `UnknownVariable of string
           ] )
         M.t
end

module InterpretResult : sig
  val eval_bop
    :  Ast.bop * value * value
    -> (value, [> `DivisionByZero | `InvalidType of value ]) result

  val eval_pattern
    :  (string, value, 'a) Base.Map.t
    -> Ast.pattern * value
    -> (string, value, 'a) Base.Map.t option

  val eval_const : Ast.const -> (value, 'a) result

  val seek
    :  (string, value, 'a) Base.Map.t
    -> string
    -> (value, [> `UnknownVariable of string ]) result

  val eval_expr
    :  (string, value, Base.String.comparator_witness) Base.Map.t
    -> Ast.expr
    -> ( value
         , [> `DivisionByZero
           | `InvalidType of value
           | `MatchFailure
           | `NotImplemented
           | `UnknownVariable of string
           ] )
         result

  val eval_decl
    :  (string, value, Base.String.comparator_witness) Base.Map.t
    -> Ast.decl
    -> ( (string, value, Base.String.comparator_witness) Base.Map.t
         , [> `DivisionByZero
           | `InvalidType of value
           | `MatchFailure
           | `NotImplemented
           | `UnknownVariable of string
           ] )
         result

  val eval_program
    :  Ast.program
    -> ( (string, value, Base.String.comparator_witness) Base.Map.t
         , [> `DivisionByZero
           | `InvalidType of value
           | `MatchFailure
           | `NotImplemented
           | `UnknownVariable of string
           ] )
         result
end

val pp_env
  :  (string, 'a * Typing.typ, 'b) Base.Map.t
  -> (string, value, 'c) Base.Map.t
  -> unit

val interpret
  :  Ast.program
  -> ( (string, value, Base.String.comparator_witness) Base.Map.t
       , [> `DivisionByZero
         | `InvalidType of value
         | `MatchFailure
         | `NotImplemented
         | `UnknownVariable of string
         ] )
       result
