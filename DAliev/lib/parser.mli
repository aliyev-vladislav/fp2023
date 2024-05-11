(** Copyright 2023-2024, Vladislav Aliev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val is_keyword : string -> bool
val parse_space : string Angstrom.t
val parse_token : string -> string Angstrom.t
val parse_parens : 'a Angstrom.t -> 'a Angstrom.t
val parse_sqparens : 'a Angstrom.t -> 'a Angstrom.t
val parse_bool_const : Ast.const Angstrom.t
val parse_string_const : Ast.const Angstrom.t
val parse_int_const : Ast.const Angstrom.t
val parse_unit_const : Ast.const Angstrom.t
val parse_nil_const : Ast.const Angstrom.t
val parse_const : Ast.const Angstrom.t
val parse_const_pat : Ast.pattern Angstrom.t
val parse_const_expr : Ast.expr Angstrom.t
val parse_mult : (Ast.expr -> Ast.expr -> Ast.expr) Angstrom.t
val parse_div : (Ast.expr -> Ast.expr -> Ast.expr) Angstrom.t
val parse_add : (Ast.expr -> Ast.expr -> Ast.expr) Angstrom.t
val parse_sub : (Ast.expr -> Ast.expr -> Ast.expr) Angstrom.t
val parse_eq : (Ast.expr -> Ast.expr -> Ast.expr) Angstrom.t
val parse_lt : (Ast.expr -> Ast.expr -> Ast.expr) Angstrom.t
val parse_lte : (Ast.expr -> Ast.expr -> Ast.expr) Angstrom.t
val parse_neq : (Ast.expr -> Ast.expr -> Ast.expr) Angstrom.t
val parse_gt : (Ast.expr -> Ast.expr -> Ast.expr) Angstrom.t
val parse_gte : (Ast.expr -> Ast.expr -> Ast.expr) Angstrom.t
val parse_and : (Ast.expr -> Ast.expr -> Ast.expr) Angstrom.t
val parse_or : (Ast.expr -> Ast.expr -> Ast.expr) Angstrom.t
val chainl1 : 'a Angstrom.t -> ('a -> 'a -> 'a) Angstrom.t -> 'a Angstrom.t
val chainr1 : 'a Angstrom.t -> ('a -> 'a -> 'a) Angstrom.t -> 'a Angstrom.t
val parse_identifier : string Angstrom.t
val parse_id_pat : Ast.pattern Angstrom.t
val parse_id_expr : Ast.expr Angstrom.t
val parse_wild_pat : Ast.pattern Angstrom.t
val parse_tuple_pat : Ast.pattern Angstrom.t -> Ast.pattern Angstrom.t
val parse_tuple_expr : Ast.expr Angstrom.t -> Ast.expr Angstrom.t
val parse_list_pat : Ast.pattern Angstrom.t -> Ast.pattern Angstrom.t
val parse_list_expr : Ast.expr Angstrom.t -> Ast.expr Angstrom.t
val parse_if_expr : Ast.expr Angstrom.t -> Ast.expr Angstrom.t
val parse_pattern : Ast.pattern Angstrom.t
val parse_match_expr : Ast.expr Angstrom.t -> Ast.expr Angstrom.t
val efunf : Ast.pattern list -> Ast.expr -> Ast.expr
val parse_fun_expr : Ast.expr Angstrom.t -> Ast.expr Angstrom.t
val parse_let_expr : Ast.expr Angstrom.t -> Ast.expr Angstrom.t
val parse_expr : Ast.expr Angstrom.t
val parse_decl : Ast.decl Angstrom.t
val parse_program : Ast.program Angstrom.t
val parse : string -> (Ast.program, string) result
val parse_input : string -> (Ast.program, string) result
