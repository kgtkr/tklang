module M = Map.Make(Expr_id)

type t = Type.t M.t

let empty: t = M.empty;;

let add (id : Expr_id.t) (typ: Type.t) (ets: t): t = M.add id typ ets;;

let merge (b: t) (a: t): t = a |> M.add_seq (M.to_seq b);;

let singleton (id: Expr_id.t) (typ: Type.t): t = M.singleton id typ;;

let get_exn (id: Expr_id.t) (ets: t): Type.t = M.find id ets

let get (id: Expr_id.t) (ets: t): Type.t option = M.find_opt id ets

let apply (f: Type.t -> Type.t) (ets: t): t = M.map f ets
