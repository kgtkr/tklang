exception UnifyError

type t = (Type.t * Type.t) list

let from_sub (s: Type_sub.t): t = s |> Type_sub.to_list |> List.map (fun (a, b) -> (Type.TypeVar a, b))

let sub (s: Type_sub.t) (equ: t) = equ |> List.map (fun (a, b) -> (Type_sub.sub_type s a, Type_sub.sub_type s b))

let rec unify (equs: t): Type_sub.t = match equs with
    | [] -> Type_sub.empty
    | (t1, t2) :: equs when Type.eq t1 t2 -> unify equs
    | ((TypeVar id, t) | (t, TypeVar id)) :: equs -> if Type_var_ids.includes id (Type.ftv t) then raise UnifyError else Type_sub.add id t (unify (sub (Type_sub.singleton id t) equs))
    | (Func(t11, t12), Func(t21, t22)) :: equs -> unify ((t11, t21) :: (t12, t22) :: equs)
    | _ -> raise UnifyError

let closure (typ: Type.t) (tenv: Type_env.t): Type_scheme.t = (Type.ftv typ |> Type_var_ids.diff (Type_env.ftv tenv), typ)

let rec pt (tenv: Type_env.t) (expr: (Var_id.t, Expr_id.t) Expr.t) (gen: Type_var_id_gen.t): (Type_sub.t * Type.t * Type_var_id_gen.t * Expr_types.t * Type_env.t) = match expr with
    | Int (nid, _) -> 
        let typ = Type.Int in
        (Type_sub.empty, typ, gen, Expr_types.singleton nid typ, Type_env.empty)
    | Var (nid, id) ->
        let (tvars, typ) = Type_env.get_exn id tenv in
        let (s, gen) = tvars
            |> Type_var_ids.to_list
            |> List.fold_left (fun (s, gen) tvar -> let (t, gen) = Type_var_id_gen.gen_type gen in (Type_sub.add tvar t s, gen)) (Type_sub.empty, gen) in
        let typ = Type_sub.sub_type s typ in
        (Type_sub.empty, Type_sub.sub_type s typ, gen, Expr_types.singleton nid typ, Type_env.empty)
    | Let (nid, id, e1, e2) ->
        let (s1, t1, gen, ets1, idents1) = pt tenv e1 gen in
        let t1c = closure t1 (Type_sub.sub_env s1 tenv) in
        let (s2, t2, gen, ets2, idents2) = pt (Type_env.add id t1c tenv) e2 gen in
        let s3 = unify (List.concat [from_sub s1; from_sub s2]) in
        let typ = Type_sub.sub_type s3 t2 in
        (s3, typ, gen, ets1 |> Expr_types.merge ets2 |> Expr_types.add nid typ, idents1 |> Type_env.merge idents2 |> Type_env.add id t1c)
    | Fun (nid, id, e) ->
        let (t1, gen) = Type_var_id_gen.gen_type gen in
        let (s, t2, gen, ets1, idents1) = pt (Type_env.add id (Type_scheme.from_type t1) tenv) e gen in
        let typ =  Type.Func(Type_sub.sub_type s t1, t2) in
        (s, typ, gen, ets1 |> Expr_types.add nid typ, idents1 |> Type_env.add id (Type_scheme.from_type t1))
    | Ap (nid, e1, e2) ->
        let (s1, t1, gen, ets1, idents1) = pt tenv e1 gen in
        let (s2, t2, gen, ets2, idents2) = pt tenv e2 gen in
        let (t3, gen) = Type_var_id_gen.gen_type gen in
        let s3 = unify ((t1, Type.Func(t2, t3)) :: List.concat [from_sub s1; from_sub s2]) in
        let typ = Type_sub.sub_type s3 t3 in
        (s3, typ, gen, ets1 |> Expr_types.merge ets2 |> Expr_types.add nid typ, idents1 |> Type_env.merge idents2)
