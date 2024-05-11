(** Copyright 2023-2024, Vladislav Aliev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type id = string

val equal_id : id -> id -> bool
val pp_id : Format.formatter -> id -> unit
val show_id : id -> string

type rec_flag =
  | Rec
  | NonRec

val equal_rec_flag : rec_flag -> rec_flag -> bool
val pp_rec_flag : Format.formatter -> rec_flag -> unit
val show_rec_flag : rec_flag -> string

type const =
  | CUnit
  | CBool of bool
  | CString of id
  | CInt of int
  | CNil

val equal_const : const -> const -> bool
val pp_const : Format.formatter -> const -> unit
val show_const : const -> string

type bop =
  | Add
  | Sub
  | Mult
  | Div
  | Eq
  | Gt
  | Gte
  | Lt
  | Lte
  | Neq
  | Or
  | And

val equal_bop : bop -> bop -> bool
val pp_bop : Format.formatter -> bop -> unit
val show_bop : bop -> string

type pattern =
  | PWild
  | PId of id
  | PConst of const
  | PTuple of pattern list
  | PCons of pattern * pattern

val equal_pattern : pattern -> pattern -> bool
val pp_pattern : Format.formatter -> pattern -> unit
val show_pattern : pattern -> string

type expr =
  | EId of id
  | EConst of const
  | EBinOp of bop * expr * expr
  | EIf of expr * expr * expr
  | EApp of expr * expr
  | EMatch of expr * (pattern * expr) list
  | EFun of pattern * expr
  | ETuple of expr list
  | ELet of rec_flag * binding * expr
  | ECons of expr * expr

and binding = pattern * expr

val equal_expr : expr -> expr -> bool
val equal_binding : binding -> binding -> bool
val pp_expr : Format.formatter -> expr -> unit
val show_expr : expr -> string
val pp_binding : Format.formatter -> binding -> unit
val show_binding : binding -> string

type decl = DLet of rec_flag * binding

val equal_decl : decl -> decl -> bool
val pp_decl : Format.formatter -> decl -> unit
val show_decl : decl -> string

type program = decl list

val equal_program : program -> program -> bool
val pp_program : Format.formatter -> program -> unit
val show_program : program -> string
val cbool : bool -> const
val cstring : string -> const
val cint : int -> const
val pid : id -> pattern
val pconst : const -> pattern
val ptuple : pattern list -> pattern
val pcons : pattern -> pattern -> pattern
val eid : id -> expr
val econst : const -> expr
val ebinop : bop -> expr -> expr -> expr
val eif : expr -> expr -> expr -> expr
val eapp : expr -> expr -> expr
val ematch : expr -> (pattern * expr) list -> expr
val efun : pattern -> expr -> expr
val etuple : expr list -> expr
val elet : rec_flag -> binding -> expr -> expr
val econs : expr -> expr -> expr
