module M = Map.Make(Var_id)

type t = Type_scheme.t M.t

let ftv (tenv: t): Type_var_ids.t = M.fold (fun _  typ acc -> Type_var_ids.merge (Type_scheme.ftv typ) acc) tenv Type_var_ids.empty

let add (id: Var_id.t) (typ: Type_scheme.t) (tenv: t): t = M.add id typ tenv

let empty: t = M.empty

let merge (tenv2: t) (tenv1: t): t = tenv1 |> M.add_seq (M.to_seq tenv2)
 
let get_exn: Var_id.t -> t -> Type_scheme.t = M.find

let get: Var_id.t -> t -> Type_scheme.t option = M.find_opt

let apply (f: Type_scheme.t -> Type_scheme.t) (tenv: t): t = M.map f tenv

let singleton: Var_id.t -> Type_scheme.t -> t = M.singleton
