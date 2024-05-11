(** Copyright 2023-2024, Vladislav Aliev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base
open Angstrom

let is_keyword = function
  | "let"
  | "in"
  | "if"
  | "then"
  | "else"
  | "fun"
  | "rec"
  | "true"
  | "false"
  | "match"
  | "with" -> true
  | _ -> false
;;

let parse_space = take_while Char.is_whitespace
let parse_token str = parse_space *> string str
let parse_parens p = parse_token "(" *> p <* parse_token ")"
let parse_sqparens p = parse_token "[" *> p <* parse_token "]"

(****************************************************** Consts ******************************************************)

let parse_bool_const =
  let t = parse_token "true" *> return (cbool true) in
  let f = parse_token "false" *> return (cbool false) in
  choice [ t; f ]
;;

let parse_string_const =
  parse_token "\""
  *> take_while (function
    | '"' -> false
    | _ -> true)
  <* char '"'
  >>| cstring
;;

let parse_int_const =
  let* sign = choice [ parse_token "-"; parse_token "+"; parse_token "" ] in
  let* num = take_while1 Char.is_digit in
  return @@ Int.of_string (sign ^ num) >>| cint
;;

let parse_unit_const = parse_token "()" *> return CUnit
let parse_nil_const = parse_token "[]" *> return CNil

let parse_const =
  choice
    [ parse_int_const
    ; parse_bool_const
    ; parse_string_const
    ; parse_unit_const
    ; parse_nil_const
    ]
;;

let parse_const_pat = parse_const >>| pconst
let parse_const_expr = parse_const >>| econst

(****************************************************** Binary ops ******************************************************)
let parse_mult = parse_token "*" *> return (ebinop Mult)
let parse_div = parse_token "/" *> return (ebinop Div)
let parse_add = parse_token "+" *> return (ebinop Add)
let parse_sub = parse_token "-" *> return (ebinop Sub)
let parse_eq = parse_token "=" *> return (ebinop Eq)
let parse_lt = parse_token "<" *> return (ebinop Lt)
let parse_lte = parse_token "<=" *> return (ebinop Lte)
let parse_neq = parse_token "<>" *> return (ebinop Neq)
let parse_gt = parse_token ">" *> return (ebinop Gt)
let parse_gte = parse_token ">=" *> return (ebinop Gte)
let parse_and = parse_token "&&" *> return (ebinop And)
let parse_or = parse_token "||" *> return (ebinop Or)

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= go
;;

let chainr1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op (e >>= go) <|> return acc in
  e >>= go
;;

(****************************************************** Identifiers ******************************************************)

let parse_identifier =
  let* fst =
    parse_space
    *> satisfy (function
      | 'a' .. 'z' | '_' -> true
      | _ -> false)
  in
  let* rest =
    take_while (fun ch -> Char.is_alphanum ch || Char.is_digit ch || Char.equal ch '_')
  in
  match String.of_char fst ^ rest with
  | var when is_keyword var -> fail "Variable name matches the keyword"
  | "_" -> fail "Variable name is the same as Wildcard"
  | _ as id -> return id
;;

let parse_id_pat = parse_identifier >>| pid
let parse_id_expr = parse_identifier >>| eid

(****************************************************** Wild ******************************************************)

let parse_wild_pat = parse_token "_" *> return PWild

(****************************************************** Tuple ******************************************************)

let parse_tuple_pat p = lift2 List.cons p (many1 (parse_token "," *> p)) >>| ptuple
let parse_tuple_expr p = lift2 List.cons p (many1 (parse_token "," *> p)) >>| etuple

(****************************************************** List ******************************************************)

let parse_list_pat p =
  parse_sqparens @@ sep_by1 (parse_token ";") p
  >>| List.fold_right ~f:pcons ~init:(pconst CNil)
;;

let parse_list_expr p =
  parse_sqparens @@ sep_by1 (parse_token ";") p
  >>| List.fold_right ~f:econs ~init:(econst CNil)
;;

(****************************************************** Branching ******************************************************)

let parse_if_expr pe =
  fix
  @@ fun peif ->
  lift3
    eif
    (parse_token "if" *> (peif <|> pe))
    (parse_token "then" *> (peif <|> pe))
    (parse_token "else" *> (peif <|> pe))
;;

(****************************************************** Pattern parsing ******************************************************)

let parse_pattern =
  fix
  @@ fun pat ->
  let term =
    choice
      [ parse_const_pat
      ; parse_id_pat
      ; parse_wild_pat
      ; parse_list_pat pat
      ; parse_parens pat
      ]
  in
  let cons = chainr1 term (parse_token "::" *> return pcons) in
  let tuple = parse_tuple_pat cons <|> cons in
  tuple
;;

(****************************************************** List matching ******************************************************)

let parse_match_expr pe =
  let pexpr =
    parse_token "match" *> pe <* parse_token "with" <* option "" (parse_token "|")
  in
  let pcase = lift2 (fun p e -> p, e) (parse_pattern <* parse_token "->") pe in
  lift2 ematch pexpr (sep_by1 (parse_token "|") pcase)
;;

(****************************************************** Anonymous functions ******************************************************)

let efunf ps e = List.fold_right ps ~f:efun ~init:e

let parse_fun_expr pe =
  lift2
    efun
    (parse_token "fun" *> parse_pattern)
    (lift2 efunf (many parse_pattern <* parse_token "->") pe)
;;

(****************************************************** let-bindings ******************************************************)

let parse_let_expr pe =
  lift3
    elet
    (parse_token "let" *> option NonRec (parse_token "rec" *> return Rec))
    (both parse_pattern (lift2 efunf (many parse_pattern <* parse_token "=") pe))
    (parse_token "in" *> pe)
;;

(****************************************************** Expression parsing ******************************************************)

let parse_expr =
  fix
  @@ fun parse_expr ->
  let expr =
    choice
      [ parse_id_expr
      ; parse_const_expr
      ; parse_list_expr parse_expr
      ; parse_parens parse_expr
      ; parse_let_expr parse_expr
      ; parse_match_expr parse_expr
      ; parse_fun_expr parse_expr
      ]
  in
  let expr = chainl1 expr (return eapp) in
  let expr = chainl1 expr (parse_mult <|> parse_div) in
  let expr = chainl1 expr (parse_add <|> parse_sub) in
  let expr = chainr1 expr (parse_token "::" *> return econs) in
  let expr = chainl1 expr (choice [ parse_lte; parse_neq; parse_lt; parse_eq ]) in
  let expr = chainl1 expr (parse_gte <|> parse_gt) in
  let expr = chainr1 expr parse_and in
  let expr = chainr1 expr parse_or in
  let expr = parse_tuple_expr expr <|> expr in
  let expr = parse_if_expr expr <|> expr in
  expr
;;

(****************************************************** Declaration ******************************************************)

let parse_decl =
  let dlet f b = DLet (f, b) in
  let parse_dlet =
    lift2
      dlet
      (parse_token "let" *> option NonRec (parse_token "rec" *> return Rec))
      (both
         parse_pattern
         (lift2 efunf (many parse_pattern <* parse_token "=") parse_expr))
  in
  parse_dlet
;;

let parse_program : program t = sep_by (parse_token ";;") parse_decl
let parse str = parse_string ~consume:Prefix parse_program str

let parse_input s =
  match parse s with
  | Ok v -> Ok v
  | Error msg -> Error msg
;;
