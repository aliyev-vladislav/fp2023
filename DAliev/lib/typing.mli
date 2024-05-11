(** Copyright 2023-2024, Vladislav Aliev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type type_var = int

type typ =
  | TBase of string
  | TVar of type_var
  | TArr of typ * typ
  | TTuple of typ list
  | TList of typ

val tbool : typ
val tstring : typ
val tint : typ
val tunit : typ
val tvar : type_var -> typ
val tarrow : typ -> typ -> typ
val ttuple : typ list -> typ
val tlist : typ -> typ
val pp_type : Format.formatter -> typ -> unit

type error =
  [ `MatchFailure
  | `NotImplemented
  | `OccursCheckFailed
  | `UnificationFailed of typ * typ
  | `UnknownVariable of string
  ]

val pp_error
  :  Format.formatter
  -> [< `MatchFailure
     | `NotImplemented
     | `OccursCheckFailed
     | `UnificationFailed of typ * typ
     | `UnknownVariable of string
     ]
  -> unit
