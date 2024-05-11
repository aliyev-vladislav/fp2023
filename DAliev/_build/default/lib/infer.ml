open Base 
open Ast 
open Typing 


type fresh = int

module R = struct
  open Result

  type 'a t = int -> int * ('a, error) Result.t

  let ( >>= ) monad f state =
    let last, result = monad state in
     match result with
     | Error err -> last, fail err
     | Ok value -> f value last
  ;;

  let ( >>| ) = fun monad f state ->
    match monad state with
    | state, Result.Error err -> state, fail err
    | state, Ok value -> state, return (f value)
  ;;

  let fail error state = state, fail error
  let return value last = last, return value
  let bind value ~f = value >>= f

  module Syntax = struct
    let ( let* ) x f = x >>= f
  end

  let fresh last = last + 1, Ok last
  let run monad = snd (monad 0)

  module RMap = struct
    let fold_left map ~init ~f =
      Map.fold map ~init ~f:(fun ~key ~data acc ->
        let open Syntax in
        let* acc = acc in
        f key data acc)
    ;;
  end

  type fresh = int
  
end
  
module Type = struct
  type t = typ

  let rec occurs_in v = function
    | TVar b -> b = v
    | TArr (l, r) -> occurs_in v l || occurs_in v r
    | TTuple typ_list -> List.exists typ_list ~f:(occurs_in v)
    | TList typ -> occurs_in v typ
    | TBase _ -> false
  ;;

  let free_vars =
    let empty = Set.empty (module Int) in
    let rec helper acc = function
      | TVar n -> Set.add acc n
      | TArr (left, right) -> helper (helper acc left) right
      | TTuple typ_list ->
        Base.List.fold_right
          typ_list
          ~f:(fun typ acc -> Set.union (helper empty typ) acc)
          ~init:acc
      | TList typ -> helper acc typ
      | TBase _ -> acc
    in
    helper empty
  ;;
end

module Subst = struct
  open R
  open R.Syntax

  type t = (fresh, typ, Int.comparator_witness) Map.t

  let empty = Map.empty (module Int)

  let mapping key value =
    if Type.occurs_in key value
    then fail `OccursCheckFailed
    else return (key, value)
  ;;

  let singleton key value =
    let* key, value = mapping key value in
    return @@ Map.update empty key ~f:(fun _ -> value)
  ;;

  let find subst key = Map.find subst key
  let remove subst key = Map.remove subst key

  let apply s =
    let rec helper = function
      | TVar n ->
        (match find s n with
         | None -> tvar n
         | Some x -> x)
      | TArr (left, right) -> tarrow (helper left) (helper right)
      | TTuple typ_list -> ttuple @@ List.map typ_list ~f:helper
      | TList typ -> tlist @@ helper typ
      | base -> base
    in
    helper
  ;;

  let rec unify l r =
    match l, r with
    | TBase l, TBase r when String.equal l r -> return empty
    | TVar a, TVar b when a = b -> return empty
    | TVar b, typ | typ, TVar b -> singleton b typ
    | TArr (f1, s1), TArr (f2, s2) ->
      let* subst1 = unify f1 f2 in
      let* subst2 = unify (apply subst1 s1) (apply subst1 s2) in
      compose subst1 subst2
    | TTuple typ_list_l, TTuple typ_list_r ->
      (match List.zip typ_list_l typ_list_r with
       | List.Or_unequal_lengths.Unequal_lengths -> fail (`UnificationFailed (l, r))
       | List.Or_unequal_lengths.Ok zipped_list ->
         List.fold_right
           zipped_list
           ~f:(fun (typ_l, typ_r) subst ->
             let* subst_pair = unify typ_l typ_r in
             let* subst = subst in
             compose subst_pair subst)
           ~init:(return empty))
    | TList typ1, TList typ2 -> unify typ1 typ2
    | _ -> fail (`UnificationFailed (l, r))

  and extend key value subst =
    match find subst key with
    | None ->
      let value = apply subst value in
      let* s2 = singleton key value in
      RMap.fold_left
        subst
        ~f:(fun key value acc ->
          let value = apply s2 value in
          let* key, value = mapping key value in
          return @@ Map.update acc key ~f:(fun _ -> value))
        ~init:(return s2)
    | Some v2 ->
      let* s2 = unify value v2 in
      compose subst s2

  and compose s1 s2 =
    RMap.fold_left s2 ~init:(return s1) ~f:extend
  ;;

  let compose_all substs =
    Base.List.fold_left substs ~init:(return empty) ~f:(fun acc subst ->
      let* acc = acc in
      compose acc subst)
  ;;
end

type scheme = (type_var, Base.Int.comparator_witness) Base.Set.t * typ

module Scheme = struct
  type t = scheme

  let empty = Set.empty (module Int)
  let occurs_in key (set, typ) = (not (Set.mem set key)) && Type.occurs_in key typ
  let free_vars (set, typ) = Set.diff set (Type.free_vars typ)

  let apply subst (set, typ) =
    let s2 = Set.fold set ~init:subst ~f:(fun acc k -> Subst.remove acc k) in
    set, Subst.apply s2 typ
  ;;
end

module TypeEnv = struct
  type t = (id, scheme, String.comparator_witness) Map.t

  let extend env id scheme = Map.update env id ~f:(fun _ -> scheme)
  let remove env key = Map.remove env key
  let empty = Map.empty (module String)

  let free_vars : t -> (type_var, Int.comparator_witness) Set.t =
    Map.fold
      ~init:(Set.empty (module Int))
      ~f:(fun ~key:_ ~data acc -> Set.union acc (Scheme.free_vars data))
  ;;

  let apply s env = Map.map env ~f:(Scheme.apply s)
  let find x env = Map.find env x
end

open R
open R.Syntax

let unify = Subst.unify

let fresh_var =
  let* fresh = fresh in
  return @@ tvar fresh
;;

let instantiate =
  let fold_right set init f =
    Set.fold_right set ~init ~f:(fun x acc ->
      let* acc = acc in
      f acc x)
  in
  fun (set, typ) ->
    fold_right set (return typ) (fun typ name ->
      let* fresh_var = fresh_var in
      let* subst = Subst.singleton name fresh_var in
      return @@ Subst.apply subst typ)
;;

let generalize env typ =
  let free = Set.diff (Type.free_vars typ) (TypeEnv.free_vars env) in
  free, typ
;;


let generalize_rec env ty x =
  let env = TypeEnv.remove env x in
  generalize env ty
;;

let lookup_env id map =
  match Map.find map id with
  | None -> fail (`UnknownVariable id)
  | Some scheme ->
    let* ans = instantiate scheme in
    return (Subst.empty, ans)
;;

let infer_pat pat =
  let rec helper env = function
    | PWild -> let* fresh = fresh_var in return (env, fresh)
    | PConst x ->
      let* typ =
        match x with
        | CInt _ -> return tint
        | CBool _ -> return tbool
        | CString _ -> return tstring
        | CNil ->
          let* fresh_variable = fresh_var in
          return (tlist fresh_variable)
        | CUnit -> return tunit
      in
      return (env, typ)
    | PId x ->
      let* fresh_var = fresh_var in
      let env = TypeEnv.extend env x (Scheme.empty, fresh_var) in
      return (env, fresh_var)
    | PCons (hd, tl) ->
      let* env1, typ_hd = helper env hd in
      let* env, typ_tl = helper env1 tl in
      let* subst = unify (tlist typ_hd) typ_tl in
      let env = TypeEnv.apply subst env in
      return (env, tlist (Subst.apply subst typ_hd))
    | PTuple tuple ->
      let rec tuple_pat env acc = function
        | hd :: tl ->
          let* env, typ_hd = helper env hd in
          tuple_pat env (typ_hd :: acc) tl
        | [] -> return (env, ttuple (List.rev acc))
      in
      tuple_pat env [] tuple
  in
  helper pat
;;

let infer_expr =
  let rec infer env = function
    | EConst c ->
      let* typ =
        match c with
        | CInt _ -> return tint
        | CString _ -> return tstring
        | CBool _ -> return tbool
        | CNil ->
          let* fresh_var = fresh_var in
          return (tlist fresh_var)
        | CUnit -> return tunit
      in
      return (Subst.empty, typ)
    | EBinOp (binop, left, right) ->
      let* subst_left, typ_left = infer env left in
      let* subst_right, typ_right = infer (TypeEnv.apply subst_left env) right in
      let* subst_result = Subst.compose_all [ subst_right; subst_left ] in
      (match binop with
      | Add | Sub | Mult | Div ->
        let* subst_1 = unify typ_left tint in
        let* subst_2 = unify typ_right tint in
        let* subst_result = Subst.compose_all [ subst_1; subst_2; subst_result ] in
        return (subst_result, tint)
      | Eq | Neq | Gt | Gte | Lt | Lte ->
        let* subst = unify typ_left typ_right in
        let* subst_result = Subst.compose_all [ subst; subst_result ] in
        return (subst_result, tbool)
      | And | Or ->
        let* subst_1 = unify typ_left tbool in
        let* subst_2 = unify typ_right tbool in
        let* subst_result = Subst.compose_all [ subst_1; subst_2; subst_result ] in
        return (subst_result, tbool))
    | EId id ->
      (match TypeEnv.find id env with
      | None -> fail (`UnknownVariable id)
      | Some s ->
        let* t = instantiate s in
        return (Subst.empty, t))
    | EApp (e1, e2) ->
      let* subst_1, typ_1 = infer env e1 in
      let* subst_2, typ_2 = infer (TypeEnv.apply subst_1 env) e2 in
      let* type_variable = fresh_var in
      let* subst_3 = unify (tarrow typ_2 type_variable) (Subst.apply subst_2 typ_1) in
      let type_result = Subst.apply subst_3 type_variable in
      let* subst_result = Subst.compose_all [ subst_1; subst_2; subst_3 ] in
      return (subst_result, type_result)
    | EFun (pat, expr) ->
      let* env_pat, typ_pat = infer_pat env pat in
      let* subst_expr, typ_expr = infer env_pat expr in
      return (subst_expr, tarrow (Subst.apply subst_expr typ_pat) typ_expr)
    | EIf (cond, thn, els) ->
      let* subst_cond, typ_cond = infer env cond in
      let* subst_thn, typ_thn = infer env thn in
      let* subst_els, typ_els = infer env els in
      let* subst_1 = unify (Subst.apply subst_els typ_cond) tbool in
      let* subst_2 = unify typ_thn typ_els in
      let* subst_result =
        Subst.compose_all [ subst_cond; subst_thn; subst_els; subst_1; subst_2 ]
      in
      let type_result = Subst.apply subst_result typ_thn in
      return (subst_result, type_result)
    | ECons (hd, tl) ->
      let* subst_hd, typ_hd = infer env hd in
      let typ_lhd = tlist typ_hd in
      let* subst_tl, typ_tl = infer env tl in
      let* s3 = unify typ_lhd typ_tl in
      let* subst_result = Subst.compose_all [ subst_hd; subst_tl; s3 ] in
      return (subst_result, Subst.apply subst_result typ_lhd)
    | ETuple el ->
      let* sub, t =
        List.fold_left
          ~f:(fun acc e ->
            let* sub, t = acc in
            let* sub1, t1 = infer env e in
            let* sub2 = Subst.compose sub sub1 in
            return (sub2, t1 :: t))
          ~init:(return (Subst.empty, []))
          el
      in
      return (sub, ttuple (List.rev_map ~f:(Subst.apply sub) t))
    | EMatch (e, cl) ->
      let* sub1, t1 = infer env e in
      let env = TypeEnv.apply sub1 env in
      let* fresh = fresh_var in
      let* sub, t =
        List.fold_left
          ~f:(fun acc (pat, exp) ->
            let* sub1, t = acc in
            let* env1, pt = infer_pat env pat in
            let* sub2 = Subst.unify t1 pt in
            let env2 = TypeEnv.apply sub2 env1 in
            let* sub3, t' = infer env2 exp in
            let* sub4 = Subst.unify t' t in
            let* sub = Subst.compose_all [ sub1; sub2; sub3; sub4 ] in
            return (sub, Subst.apply sub t))
          ~init:(return (sub1, fresh))
          cl
      in
      return (sub, t)
    | ELet (NonRec, (PId x, e1), e2) ->
      let* s1, t1 = infer env e1 in
      let env = TypeEnv.apply s1 env in
      let s = generalize env t1 in
      let* s2, t2 = infer (TypeEnv.extend env x s) e2 in
      let* s = Subst.compose s1 s2 in
      return (s, t2)
    | ELet (Rec, (PId x, e1), e2) ->
      let* fresh = fresh_var in
      let env1 = TypeEnv.extend env x (Scheme.empty, fresh) in
      let* s, t = infer env1 e1 in
      let* s1 = Subst.unify t fresh in
      let* s2 = Subst.compose s s1 in
      let env = TypeEnv.apply s2 env in
      let t = Subst.apply s2 t in
      let s = generalize_rec env t x in
      let env = TypeEnv.extend env x s in
      let* sub, t = infer env e2 in
      let* sub = Subst.compose s2 sub in
      return (sub, t)
    | _ -> fail `NotImplemented 
  in
  infer
;;

let infer_decl env = function
  | DLet (NonRec, (PId id, expr)) ->
    let* subst, typ = infer_expr env expr in
    let env = TypeEnv.apply subst env in
    let env = TypeEnv.extend env id (generalize env typ) in
    return env
  | DLet (Rec, (PId id, expr)) ->
    let* type_variable = fresh_var in
    let env = TypeEnv.extend env id (Scheme.empty, type_variable) in
    let* subst, typ = infer_expr env expr in
    let* subst_2 = unify (Subst.apply subst type_variable) typ in
    let* subst = Subst.compose subst_2 subst in
    let type_variable = Subst.apply subst type_variable in
    let scheme = generalize env type_variable in
    let env = TypeEnv.extend env id scheme in
    return env
  | _ -> fail `NotImplemented
;;

let infer_program (program : program)=
  List.fold_left
    ~f:(fun acc item ->
      let* env = acc in
      let* env = infer_decl env item in
      return env)
    ~init:(return TypeEnv.empty)
    program
;;

let run_inference prog = run (infer_program prog)

let print_env env =
  let open Stdlib.Format in
  (match env with
  | Ok env ->
    Base.Map.iteri env ~f:(fun ~key ~data:(_, ty) ->
      printf "val %s : %a\n" key Typing.pp_type ty)
  | Error e -> printf "Infer error: %a\n" Typing.pp_error e)
;;

