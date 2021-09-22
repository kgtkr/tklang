module M = Map.Make(Type_var_id)

type t = Type.t M.t

let rec sub_type (s: t) (t: Type.t) = match t with
    | TypeVar id -> M.find_opt id s |> Option.value ~default: t
    | Bool -> t
    | Int -> t
    | Func (t1, t2) -> Func (sub_type s t1, sub_type s t2)
    | List t1 -> List (sub_type s t1)


(** 型スキーマの束縛変数は衝突しないという仮定が必要 *)
let sub_type_scheme (s: t) ((vars, t): Type_scheme.t): Type_scheme.t = (vars, sub_type s t)

(** 型スキーマの束縛変数は衝突しないという仮定が必要 *)
let sub_env (s: t) (e: Type_env.t): Type_env.t = Type_env.apply (sub_type_scheme s) e

let sub_expr_types (s: t) (e: Expr_types.t): Expr_types.t = Expr_types.apply (sub_type s) e

let to_list (s: t): (Type_var_id.t * Type.t) list = s |> M.to_seq |> List.of_seq

let empty: t = M.empty

let add (id: Type_var_id.t) (t: Type.t) (s: t) = M.add id t s

let singleton: Type_var_id.t -> Type.t -> t = M.singleton
