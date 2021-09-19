open Set_ext

type t = SI.t * Type.t

let ftv ((vars, t): t): SI.t = SI.diff (Type.ftv t) vars
