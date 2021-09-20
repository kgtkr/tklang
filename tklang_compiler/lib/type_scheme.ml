open Set_ext

type t = SI.t * Type.t

let to_string ((vars, t): t): string = "{" ^ (String.concat "," (vars |> SI.to_seq |> List.of_seq |> List.map Int.to_string)) ^ "}" ^ Type.to_string t

let ftv ((vars, t): t): SI.t = SI.diff (Type.ftv t) vars
