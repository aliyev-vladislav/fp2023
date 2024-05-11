(** Copyright 2023-2024, Vladislav Aliev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type id = string [@@deriving eq, show { with_path = false }]

type rec_flag =
  | Rec (** Recursive *)
  | NonRec (** Not recursive *)
[@@deriving eq, show { with_path = false }]

type const =
  | CUnit (** () *)
  | CBool of bool (** bool *)
  | CString of string (** string *)
  | CInt of int (** int *)
  | CNil (** [] *)
[@@deriving eq, show { with_path = false }]

type bop =
  | Add (** + *)
  | Sub (** - *)
  | Mult (** * *)
  | Div (** / *)
  | Eq (** = *)
  | Gt (** > *)
  | Gte (** >= *)
  | Lt (** < *)
  | Lte (** <= *)
  | Neq (** <> *)
  | Or (** || *)
  | And (** && *)
[@@deriving eq, show { with_path = false }]

type pattern =
  | PWild (** _ *)
  | PId of id (** x *)
  | PConst of const (** "ocaml" *)
  | PTuple of pattern list (** (x, y) *)
  | PCons of pattern * pattern (** hd :: tl *)
[@@deriving eq, show { with_path = false }]

type expr =
  | EId of id (** Identifier: x *)
  | EConst of const (** Const: 2; true *)
  | EBinOp of bop * expr * expr (** Binary operation: 2 + 2 *)
  | EIf of expr * expr * expr (** If-then-else: if true then ... else ... *)
  | EApp of expr * expr (** Application f x *)
  | EMatch of expr * (pattern * expr) list (** Closure: let f x = x + 1 in f 1*)
  | EFun of pattern * expr (** Function: fun x -> x + 1 *)
  | ETuple of expr list (** Tuple: (1, 2, 3) *)
  | ELet of rec_flag * binding * expr (** let x = e1 in e2 *)
  | ECons of expr * expr (** hd :: tl*)
[@@deriving eq, show { with_path = false }]

and binding = pattern * expr [@@deriving show { with_path = false }]

type decl = DLet of rec_flag * binding (** Let declarations *)
[@@deriving eq, show { with_path = false }]

(** Sequence of program dels *)
type program = decl list [@@deriving eq, show { with_path = false }]

let cbool b = CBool b
let cstring s = CString s
let cint i = CInt i
let pid x = PId x
let pconst c = PConst c
let ptuple p_list = PTuple p_list
let pcons p1 p2 = PCons (p1, p2)
let eid x = EId x
let econst c = EConst c
let ebinop op e1 e2 = EBinOp (op, e1, e2)
let eif e1 e2 e3 = EIf (e1, e2, e3)
let eapp e1 e2 = EApp (e1, e2)
let ematch e cl = EMatch (e, cl)
let efun p e = EFun (p, e)
let etuple el = ETuple el
let elet f b e = ELet (f, b, e)
let econs e1 e2 = ECons (e1, e2)
