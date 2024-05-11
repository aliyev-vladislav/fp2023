(** Copyright 2023-2024, Vladislav Aliev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type type_var = int

type typ =
  | TBase of string
  | TVar of type_var
  | TArr of typ * typ
  | TTuple of typ list
  | TList of typ

let tbool = TBase "bool"
let tstring = TBase "string"
let tint = TBase "int"
let tunit = TBase "unit"
let tvar v = TVar v
let tarrow left_type right_type = TArr (left_type, right_type)
let ttuple type_list = TTuple type_list
let tlist typ = TList typ

let rec pp_type fmt =
  let open Format in
  function
  | TBase typ -> fprintf fmt "%s" typ
  | TVar v ->
    let code_of_a = 97 in
    fprintf fmt "%s" ("'" ^ Char.escaped (Char.chr (v + code_of_a)))
  | TArr (left_type, right_type) ->
    (match left_type with
     | TArr _ -> fprintf fmt "(%a) -> %a" pp_type left_type pp_type right_type
     | _ -> fprintf fmt "%a -> %a" pp_type left_type pp_type right_type)
  | TTuple type_list ->
    fprintf
      fmt
      "(%a)"
      (pp_print_list
         ~pp_sep:(fun _ _ -> fprintf fmt " * ")
         (fun fmt typ ->
           match typ with
           | TArr _ -> fprintf fmt "(%a)" pp_type typ
           | _ -> fprintf fmt "%a" pp_type typ))
      type_list
  | TList typ -> fprintf fmt "%a list" pp_type typ
;;

type error =
  [ `OccursCheckFailed
  | `UnknownVariable of id
  | `UnificationFailed of typ * typ
  | `MatchFailure
  | `NotImplemented
  ]

let pp_error fmt =
  let open Format in
  function
  | `OccursCheckFailed -> fprintf fmt "Occurs check failed"
  | `UnknownVariable v -> fprintf fmt "Unknown variable '%s'" v
  | `UnificationFailed (t1, t2) ->
    fprintf fmt "Failed to unify types %a and %a" pp_type t1 pp_type t2
  | `MatchFailure -> fprintf fmt "Match failure"
  | `NotImplemented -> fprintf fmt "Expression contains not implemented features"
;;
