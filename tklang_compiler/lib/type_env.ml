open Set_ext
open Map_ext

type t = Type_scheme.t MI.t

let ftv (tenv: t): SI.t = MI.fold (fun _  typ acc -> SI.union (Type_scheme.ftv typ) acc) tenv SI.empty

let add (id: int) (typ: Type_scheme.t) (tenv: t): t = MI.add id typ tenv

let empty: t = MI.empty

let merge (tenv2: t) (tenv1: t): t = tenv1 |> MI.add_seq (MI.to_seq tenv2)
