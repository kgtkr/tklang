open Set_ext

type t = TypeVar of int | Bool | Int | Func of t * t | List of t

let rec to_string (x:t): string =
    match x with
    | TypeVar x -> "'" ^ Int.to_string x
    | Bool -> "bool"
    | Int -> "int"
    | Func (x, y) -> "(" ^ to_string x ^ ")->(" ^ to_string y ^ ")"
    | List x -> "[" ^ to_string x ^ "]"

let rec ftv (ts: t): SI.t = match ts with
    | TypeVar id -> SI.singleton id
    | Bool -> SI.empty
    | Int -> SI.empty
    | Func (t1, t2) -> SI.union (ftv t1) (ftv t2)
    | List t -> ftv t

let rec eq (t1: t) (t2: t): bool = match (t1, t2) with
    | (TypeVar id1, TypeVar id2) -> id1 = id2
    | (Bool, Bool) -> true
    | (Int, Int) -> true
    | (Func (t1, t2), Func (t3, t4)) -> eq t1 t3 && eq t2 t4
    | (List t1, List t2) -> eq t1 t2
    | _ -> false
