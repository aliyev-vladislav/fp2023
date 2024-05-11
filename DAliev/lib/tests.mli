(** Copyright 2023-2024, Vladislav Aliev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module ParserTests : sig
  val parse_test : string -> DAliev_lib.Ast.decl list -> bool
end

module InferTests : sig
  val infer_test : string -> unit
end

module InterpretTests : sig
  val interpret_test : string -> unit
end
