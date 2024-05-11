(** Copyright 2023-2024, Vladislav Aliev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

  open Ast
  open Base

  module type MONAD = sig
    type ('a, 'e) t
  
    val return : 'a -> ('a, 'e) t
    val fail : 'e -> ('a, 'e) t
    val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
  end

  module MONAD_RESULT = struct
    include Base.Result
  
    let ( let* ) m f = m >>= fun x -> f x
  end

  type value =
    | VUnit
    | VBool of bool 
    | VString of string
    | VInt of int
    | VTuple of value list
    | VList of value list
    | VFun of pattern * rec_flag * expr * envr

  and envr = (id, value, String.comparator_witness) Map.t



  let rec pp_value fmt = let open Stdlib.Format in 
  function
    | VUnit -> fprintf fmt "()"
    | VBool b -> fprintf fmt "%b" b
    | VString str -> fprintf fmt "%s" str
    | VInt i -> fprintf fmt "%d" i
    | VFun _ -> fprintf fmt "<fun>"
    | VTuple lst -> 
        fprintf fmt "(%a)" 
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_value) 
        lst
    | VList lst -> 
        fprintf fmt "[%a]" 
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "; ") pp_value) 
        lst
  ;;


  type error = 
  [ `DivisionByZero
  | `MatchFailure
  | `UnknownVariable of id
  | `InvalidType of value
  | `NotImplemented 
  ]


  let pp_error fmt = let open Stdlib.Format in 
  function 
    | `DivisionByZero -> fprintf fmt "Division by zero"
    | `MatchFailure -> fprintf fmt "Match failure" 
    | `UnknownVariable id -> fprintf fmt "Unknown variable %s" id
    | `InvalidType t -> fprintf fmt "Invalid type %a" pp_value t
    | `NotImplemented -> fprintf fmt "Not inplemented"
  ;;


  module Env (M: MONAD) = struct 
    open M 

    let empty = Map.empty (module String)
    let extend env k v = Map.update env k ~f:(fun _ -> v)

    let find m k = 
      match Map.find m k with 
      | Some v -> return v
      | None -> fail (`UnknownVariable k)
    ;;
  end

  module Interpret (M: MONAD) = struct
    open M
    open Env (M)


    let eval_bop (bop, v1, v2)= 
      match bop, v1, v2 with
      | Add, VInt l, VInt r -> return (VInt (l + r))
      | Sub, VInt l, VInt r -> return (VInt (l - r))
      | Mult, VInt l, VInt r -> return (VInt (l * r))
      | Div, VInt _, VInt 0 -> fail `DivisionByZero
      | Div, VInt l, VInt r -> return (VInt (l / r))
      | Eq, VInt l, VInt r -> return (VBool (l = r))
      | Gt, VInt l, VInt r -> return (VBool (l > r))
      | Gte, VInt l, VInt r -> return (VBool (l >= r))
      | Lt, VInt l, VInt r -> return (VBool (l < r))
      | Lte, VInt l, VInt r -> return (VBool (l <= r))
      | Neq, VInt l, VInt r -> return (VBool (l <> r))
      | And, VBool l, VBool r -> return (VBool (l && r))
      | Or, VBool l, VBool r -> return (VBool (l || r))
      | _ -> fail (`InvalidType v1)
     ;;

    let rec eval_pattern env = function
      | PWild, _ -> Some env
      | PId id, v -> Some (extend env id v)
      | PConst CNil, VList [] -> Some env
      | PConst (CBool b1), VBool b2 when Bool.equal b1 b2 -> Some env
      | PConst (CString str1), VString str2 when String.equal str1 str2 -> Some env
      | PConst (CInt i1), VInt i2 when i1 = i2 -> Some env
      | PTuple plist, VTuple vlist -> 
        let env = 
          List.fold2 
            plist 
            vlist 
            ~f:(fun env k v -> 
              match env with
              | Some e -> eval_pattern e (k, v)
              | None -> None) 
            ~init:(Some env)
        in
        (match env with
        | Ok env -> env
        | _ -> None)
      | PCons (pat1, pat2), VList (v :: vlist) -> 
        let env = eval_pattern env (pat2, VList vlist) in
        (match env with 
        | Some env -> eval_pattern env (pat1, v)
        | None -> None)
      | _ -> None
    ;;


    let eval_const = function
      | CUnit -> return VUnit
      | CBool b -> return (VBool b)
      | CString str -> return (VString str)
      | CInt i -> return (VInt i)
      | CNil -> return (VList [])
    ;;

    let seek env x =
      let* v = find env x in
      match v with
      | VFun (pat, Rec, expr, env) -> return (VFun (pat, Rec, expr, extend env x v))
      | _ -> return v
    ;;

    let eval_expr = 
      let rec helper env = function
        | EId v -> seek env v
        | EConst c -> eval_const c
        | EBinOp (op, expr1, expr2) -> 
          let* v1 = helper env expr1 in
          let* v2 = helper env expr2 in
          eval_bop (op, v1, v2)
        | EIf (expr1, expr2, expr3) -> 
          let* v1 = helper env expr1 in
          (match v1 with
          | VBool true -> helper env expr2
          | VBool false -> helper env expr3
          | _ -> fail (`InvalidType v1))
        | EApp (expr1, expr2) -> 
          let* v1 = helper env expr1 in
          let* v2 = helper env expr2 in 
          (match v1 with
          | VFun (pat, _, expr, env) -> 
            let* env' =
              match eval_pattern env (pat, v2) with
              | Some env -> return env
              | None -> fail `MatchFailure
             in
             helper env' expr 
          | _ -> fail (`InvalidType v1))
        | EMatch (expr, clist) ->
          let* v = helper env expr in
          let rec match_helper env v = function
            | (pat, expr) :: tl ->
              let env' = eval_pattern env (pat, v) in
              (match env' with
               | Some env -> helper env expr
               | None -> match_helper env v tl)
            | [] -> fail `MatchFailure
          in
          match_helper env v clist
        | EFun (pat, expr) -> return (VFun (pat, NonRec, expr, env))
        | ETuple elist -> 
          let* vlist =  List.fold_left 
              ~f:(fun acc expr -> 
                let* acc = acc in
                let* v = helper env expr in
              return(v :: acc))
              ~init:(return []) 
              elist
          in
          return (VTuple (List.rev vlist))
        | ELet (Rec, (PId x, e1), e2) ->
          let* v = helper env e1 in
          let env1 = extend env x v in
          let v =
            match v with
            | VFun (p, _, e, _) -> VFun (p, Rec, e, env1)
            | _ -> v
          in
          let env2 = extend env x v in
          helper env2 e2
        | ELet (NonRec, (PId x, e1), e2) ->
          let* v = helper env e1 in
          let env = extend env x v in
          helper env e2
        | ECons (h, tl) ->
          let* hv = helper env h in
          let* tlv = helper env tl in
          (match tlv with
            | VList vl -> return (VList (hv :: vl))
            | _ -> fail (`InvalidType tlv)) 
        | _ -> fail `NotImplemented
      in
      helper
    ;;


    let eval_decl env = function
    | DLet (NonRec, (PId pat, expr)) ->
      let* v = eval_expr env expr in
      let env = extend env pat v in
      return env
    | DLet (Rec, (PId pat, expr)) ->
      let* v = eval_expr env expr in
      let env1 = extend env pat v in
      let v =
        match v with
        | VFun (pat', _ , expr, _) -> VFun (pat', Rec, expr, env1)
        | _ -> v
      in
      let env = extend env pat v in
      return env
    | _ -> fail `NotImplemented
  ;;

    let eval_program (program : program) =
      List.fold_left
        ~f:(fun env decl ->
          let* env = env in
          let* env = eval_decl env decl in
          return env)
        ~init:(return empty)
        program
    ;;
  end

  module InterpretResult = Interpret (struct
    include Base.Result

    let ( let* ) m f = bind m ~f
  end)

  let pp_env env_t env_v =
    let open Stdlib.Format in
    Base.Map.iteri
      ~f:(fun ~key ~data ->
        match Base.Map.find env_t key with
        | Some (_, ty) ->
          printf "val %s : %a = %a\n" key Typing.pp_type ty pp_value data
        | None -> printf "val %s = %a\n" key pp_value data)
      env_v
  ;;
  let interpret = InterpretResult.eval_program
