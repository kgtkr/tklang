module S = Set.Make(Type_var_id);;

type t = S.t;;

let empty: t = S.empty;;

let add (x: Type_var_id.t) (s: t): t = S.add x s;;

let merge (a: t) (b: t): t = S.union a b;;

let singleton (x: Type_var_id.t): t = S.singleton x;;

let includes (x: Type_var_id.t) (s: t): bool = S.mem x s;;

let diff (b: t) (a: t): t = S.diff a b;;

let to_list (s: t): Type_var_id.t list = S.to_seq s |> List.of_seq;;
