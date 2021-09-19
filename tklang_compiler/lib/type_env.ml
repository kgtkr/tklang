open Set_ext
open Map_ext

type t = Type_scheme.t MI.t

let ftv (te: t): SI.t = MI.fold (fun _  x acc -> SI.union (Type_scheme.ftv x) acc) te SI.empty
