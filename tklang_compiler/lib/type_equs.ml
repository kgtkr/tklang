exception UnifyError

type t = (Type.t * Type.t) list

let from_sub (s: Type_sub.t): t = s |> Type_sub.to_list |> List.map (fun (a, b) -> (Type.TypeVar a, b))

let sub (s: Type_sub.t) (equ: t): t = equ |> List.map (fun (a, b) -> (Type_sub.sub_type s a, Type_sub.sub_type s b))

let rec unify (equs: t): Type_sub.t =
    match equs with
    | [] -> Type_sub.empty
    | (t1, t2) :: equs when Type.eq t1 t2 -> unify equs
    | ((TypeVar id, t) | (t, TypeVar id)) :: equs -> if Type_var_ids.includes id (Type.ftv t) then raise UnifyError else Type_sub.add id t (unify (sub (Type_sub.singleton id t) equs))
    | (Func(t11, t12), Func(t21, t22)) :: equs -> unify ((t11, t21) :: (t12, t22) :: equs)
    | (List(t1), List(t2)) :: equs -> unify ((t1, t2) :: equs)
    | _ -> raise UnifyError

let closure (typ: Type.t) (tenv: Type_env.t): Type_scheme.t = (Type.ftv typ |> Type_var_ids.diff (Type_env.ftv tenv), typ)

let rec pt_pat_helper_ (p: Var_id.t Expr.pat) (tenv: Type_env.t) (gen: Type_var_id_gen.t): (Type_sub.t * Type.t * Type_env.t * Type_var_id_gen.t) =
    match p with
    | AnyPat id ->
        let (t, gen) = Type_var_id_gen.gen_type gen in
        (Type_sub.empty, t, tenv |> Type_env.add id (Type_scheme.from_type t), gen)
    | EmptyListPat ->
        let (t, gen) = Type_var_id_gen.gen_type gen in
        (Type_sub.empty, Type.List t, tenv, gen)
    | ConsPat (p1, p2) ->
        let (s1, t1, tenv, gen) = pt_pat_helper_ p1 tenv gen in
        let (s2, t2, tenv, gen) = pt_pat_helper_ p2 tenv gen in
        let s3 = unify ((Type.List t1, t2) :: List.concat [from_sub s1; from_sub s2]) in
        (s3, Type_sub.sub_type s3 t2, tenv, gen)
    | IgnorePat ->
        let (t, gen) = Type_var_id_gen.gen_type gen in
        (Type_sub.empty, t, tenv, gen)

let pt_pat_helper (p: Var_id.t Expr.pat) (gen: Type_var_id_gen.t): (Type_sub.t * Type.t * Type_env.t * Type_var_id_gen.t) =
    pt_pat_helper_ p Type_env.empty gen

let rec pt_ (tenv: Type_env.t) (expr: (Var_id.t, Expr_id.t) Expr.t) (gen: Type_var_id_gen.t) (ets: Expr_types.t) (tenv_acc: Type_env.t): (Type_sub.t * Type.t * Type_var_id_gen.t * Expr_types.t * Type_env.t) = match expr with
    | Int (nid, _) -> 
        let typ = Type.Int in
        (Type_sub.empty, typ, gen, ets |> Expr_types.add nid typ, tenv_acc)
    | Bool (nid, _) ->
        let typ = Type.Bool in
        (Type_sub.empty, typ, gen, ets |> Expr_types.add nid typ, tenv_acc)
    | Var (nid, id) ->
        let (tvars, typ) = Type_env.get_exn id tenv in
        let (s, gen) = tvars
            |> Type_var_ids.to_list
            |> List.fold_left (fun (s, gen) tvar -> let (t, gen) = Type_var_id_gen.gen_type gen in (Type_sub.add tvar t s, gen)) (Type_sub.empty, gen) in
        let typ = Type_sub.sub_type s typ in
        (Type_sub.empty, Type_sub.sub_type s typ, gen, ets |> Expr_types.add nid typ, tenv_acc)
    | Op (nid, e1, op, e2) ->
        let (s1, t1, gen, ets, tenv_acc) = pt_ tenv e1 gen ets tenv_acc in
        let (s2, t2, gen, ets, tenv_acc) = pt_ tenv e2 gen ets tenv_acc in
        let s3 = unify ((t1, Type.Int) :: (t2, Type.Int) :: List.concat [from_sub s1; from_sub s2]) in
        let t3 = (match op with Lt -> Type.Bool | _ -> Type.Int) in
        let typ = Type_sub.sub_type s3 t3 in
        (s3, typ, gen, ets |> Expr_types.add nid typ, tenv_acc)
    | If (nid, e1, e2, e3) ->
        let (s1, t1, gen, ets, tenv_acc) = pt_ tenv e1 gen ets tenv_acc in
        let (s2, t2, gen, ets, tenv_acc) = pt_ tenv e2 gen ets tenv_acc in
        let (s3, t3, gen, ets, tenv_acc) = pt_ tenv e3 gen ets tenv_acc in
        let s4 = unify ((t1, Type.Bool) :: (t2, t3) :: List.concat [from_sub s1; from_sub s2; from_sub s3]) in
        let typ = Type_sub.sub_type s4 t2 in
        (s4, typ, gen, ets |> Expr_types.add nid typ, tenv_acc)
    | Let (nid, id, e1, e2) ->
        let (s1, t1, gen, ets, tenv_acc) = pt_ tenv e1 gen ets tenv_acc in
        let t1c = closure t1 (Type_sub.sub_env s1 tenv) in
        let (s2, t2, gen, ets, tenv_acc) = pt_ (Type_env.add id t1c tenv) e2 gen ets tenv_acc in
        let s3 = unify (List.concat [from_sub s1; from_sub s2]) in
        let typ = Type_sub.sub_type s3 t2 in
        (s3, typ, gen, ets |> Expr_types.add nid typ, tenv_acc |> Type_env.add id t1c)
    | Fun (nid, id, e) ->
        let (t1, gen) = Type_var_id_gen.gen_type gen in
        let (s, t2, gen, ets, tenv_acc) = pt_ (Type_env.add id (Type_scheme.from_type t1) tenv) e gen ets tenv_acc in
        let typ =  Type.Func(Type_sub.sub_type s t1, t2) in
        (s, typ, gen, ets |> Expr_types.add nid typ, tenv_acc |> Type_env.add id (Type_scheme.from_type t1))
    | Ap (nid, e1, e2) ->
        let (s1, t1, gen, ets, tenv_acc) = pt_ tenv e1 gen ets tenv_acc in
        let (s2, t2, gen, ets, tenv_acc) = pt_ tenv e2 gen ets tenv_acc in
        let (t3, gen) = Type_var_id_gen.gen_type gen in
        let s3 = unify ((t1, Type.Func(t2, t3)) :: List.concat [from_sub s1; from_sub s2]) in
        let typ = Type_sub.sub_type s3 t3 in
        (s3, typ, gen, ets |> Expr_types.add nid typ, tenv_acc)
    | LetRecFun (nid, id1, id2, e1, e2) ->
        let (t_id1, gen) = Type_var_id_gen.gen_type gen in
        let (t_id2, gen) = Type_var_id_gen.gen_type gen in
        let (s1, t1, gen, ets, tenv_acc) = pt_ (tenv |> Type_env.add id1 (Type_scheme.from_type t_id1) |> Type_env.add id2 (Type_scheme.from_type t_id2)) e1 gen ets tenv_acc in
        let s2 = unify ((t_id1, Type.Func(t_id2, t1)) :: (from_sub s1)) in
        let t_id1c = closure (Type_sub.sub_type s2 t_id1) (Type_sub.sub_env s2 tenv) in
        let (s3, t3, gen, ets, tenv_acc) = pt_ (Type_env.add id1 t_id1c tenv) e2 gen ets tenv_acc in
        let s4 = unify (List.concat [from_sub s2; from_sub s3]) in
        let typ = Type_sub.sub_type s4 t3 in
        (s4, typ, gen, ets |> Expr_types.add nid typ, tenv_acc |> Type_env.add id1 t_id1c |> Type_env.add id2 (Type_scheme.from_type t_id2))
    | EmptyList nid ->
        let (t1, gen) = Type_var_id_gen.gen_type gen in
        let typ = Type.List t1 in
        (Type_sub.empty, Type.List t1, gen, ets |> Expr_types.add nid typ, tenv_acc)
    | Cons(nid, e1, e2) ->
        let (s1, t1, gen, ets, tenv_acc) = pt_ tenv e1 gen ets tenv_acc in
        let (s2, t2, gen, ets, tenv_acc) = pt_ tenv e2 gen ets tenv_acc in
        let s3 = unify ((t2, Type.List t1) :: List.concat [from_sub s1; from_sub s2]) in
        let typ = Type_sub.sub_type s3 t2 in
        (s3, typ, gen, ets |>Expr_types.add nid typ, tenv_acc)
    | Match (nid, e1, clauses) ->
        let (s1, t1, gen, ets, tenv_acc) = pt_ tenv e1 gen ets tenv_acc in
        let (t2, gen) = Type_var_id_gen.gen_type gen in
        let (s2, gen, ets, tenv_acc) = List.fold_left (fun (s2, gen, ets, tenv_acc) (pat, e2) ->
            let (pat_s, pat_t, pat_tenv, gen) = pt_pat_helper pat gen in
            let (e2_s, e2_t, gen, ets, tenv_acc) = pt_ (Type_env.merge pat_tenv tenv) e2 gen ets tenv_acc in
            let s2 = unify ((t1, pat_t) :: (t2, e2_t) :: List.concat [from_sub pat_s; from_sub e2_s; from_sub s2]) in
            (s2, gen, ets, tenv_acc |> Type_env.merge pat_tenv)
        ) (Type_sub.empty, gen, ets, tenv_acc) clauses in
        let s3 = unify (List.concat [from_sub s1; from_sub s2]) in
        let typ = Type_sub.sub_type s3 t2 in
        (s3, typ, gen, ets |> Expr_types.add nid typ, tenv_acc)

let pt (expr: (Var_id.t, Expr_id.t) Expr.t): (Expr_types.t * Type_env.t) =
    let (s, _, _, ets, tenv_acc) = pt_ Type_env.empty expr Type_var_id_gen.make Expr_types.empty Type_env.empty in
    (Type_sub.sub_expr_types s ets, Type_sub.sub_env s tenv_acc)
