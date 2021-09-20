open Map_ext

type t = Type.t MI.t

let rec sub_type (s: t) (t: Type.t) = match t with
    | TypeVar id -> (match MI.find_opt id s with | Some x -> x | None -> t)
    | Bool -> t
    | Int -> t
    | Func (t1, t2) -> Func (sub_type s t1, sub_type s t2)
    | List t1 -> List (sub_type s t1)


(** 型スキーマの束縛変数は衝突しないという仮定が必要 *)
let sub_type_scheme (s: t) ((vars, t): Type_scheme.t): Type_scheme.t = (vars, sub_type s t)

(** 型スキーマの束縛変数は衝突しないという仮定が必要 *)
let sub_env (s: t) (e: Type_env.t): Type_env.t = MI.map (sub_type_scheme s) e
