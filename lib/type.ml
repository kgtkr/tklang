type t = TypeVar of Type_var_id.t | Int | Func of t * t

let rec to_string (x:t): string =
    match x with
    | TypeVar x -> Type_var_id.to_string x
    | Int -> "int"
    | Func (x, y) -> "(" ^ to_string x ^ " -> " ^ to_string y ^ ")"

let rec ftv (ts: t): Type_var_ids.t = match ts with
    | TypeVar id -> Type_var_ids.singleton id
    | Int -> Type_var_ids.empty
    | Func (t1, t2) -> Type_var_ids.merge (ftv t1) (ftv t2)

let rec eq (t1: t) (t2: t): bool = match (t1, t2) with
    | (TypeVar id1, TypeVar id2) -> id1 = id2
    | (Int, Int) -> true
    | (Func (t1, t2), Func (t3, t4)) -> eq t1 t3 && eq t2 t4
    | _ -> false
