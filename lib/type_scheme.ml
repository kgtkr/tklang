type t = Type_var_ids.t * Type.t

let to_string ((vars, t): t): string = "{" ^ (String.concat "," (vars |> Type_var_ids.to_list |> List.map Type_var_id.to_string)) ^ "}" ^ Type.to_string t

let ftv ((vars, t): t): Type_var_ids.t = (Type.ftv t) |> Type_var_ids.diff vars

let make (vars: Type_var_ids.t) (typ: Type.t): t = (vars, typ)

let from_type (typ: Type.t): t = (Type_var_ids.empty, typ)
