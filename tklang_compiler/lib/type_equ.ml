open Map_ext
open Set_ext

exception UnifyError

type t = (Type.t * Type.t) list

let from_sub (s: Type_sub.t): t = s |> MI.to_seq |> Seq.map (fun (a, b) -> (Type.TypeVar a, b)) |> List.of_seq

let sub (s: Type_sub.t) (equ: t) = equ |> List.map (fun (a, b) -> (Type_sub.sub_type s a, Type_sub.sub_type s b))

let rec unify (equs: t): Type_sub.t = match equs with
    | [] -> MI.empty
    | (t1, t2) :: equs when Type.eq t1 t2 -> unify equs
    | ((TypeVar id, t) | (t, TypeVar id)) :: equs -> if SI.mem id (Type.ftv t) then raise UnifyError else MI.add id t (unify (sub (MI.singleton id t) equs))
    | (Func(t11, t12), Func(t21, t22)) :: equs -> unify ((t11, t21) :: (t12, t22) :: equs)
    | (List(t1), List(t2)) :: equs -> unify ((t1, t2) :: equs)
    | _ -> raise UnifyError

let closure (typ: Type.t) (tenv: Type_env.t): Type_scheme.t = (SI.diff (Type.ftv typ) (Type_env.ftv tenv), typ)

let rec pt_pat_helper (p: int Expr.pat) (counter: int): (Type_sub.t * Type.t * Type_env.t * int) = match p with
    | AnyPat id ->
        let (t, counter) = (Type.TypeVar counter, counter + 1) in
        (MI.empty, t, MI.singleton id (SI.empty, t), counter)
    | EmptyListPat ->
        let (t, counter) = (Type.TypeVar counter, counter + 1) in
        (MI.empty, Type.List t, MI.empty, counter)
    | ConsPat (p1, p2) ->
        let (s1, t1, tenv1, counter) = pt_pat_helper p1 counter in
        let (s2, t2, tenv2, counter) = pt_pat_helper p2 counter in
        let s3 = unify ((Type.List t1, t2) :: List.concat [from_sub s1; from_sub s2]) in
        (s3, Type_sub.sub_type s3 t2, MI.add_seq (MI.to_seq tenv2) tenv1, counter)
    | IgnorePat ->
        let (t, counter) = (Type.TypeVar counter, counter + 1) in
        (MI.empty, t, MI.empty, counter)

let rec pt (tenv: Type_env.t) (expr: (int, int) Expr.t) (counter: int): (Type_sub.t * Type.t * int * Type.t MI.t * Type_scheme.t MI.t) = match expr with
    | Int (nid, _) -> 
        let typ = Type.Int in
        (MI.empty, typ, counter, MI.singleton nid typ, MI.empty)
    | Bool (nid, _) ->
        let typ = Type.Bool in
        (MI.empty, typ, counter, MI.singleton nid typ, MI.empty)
    | Var (nid, id) ->
        let (tvars, typ) = MI.find id tenv in
        let (s, counter) = tvars |> SI.to_seq |> Seq.fold_left (fun (s, counter) tvar -> (MI.add tvar (Type.TypeVar counter) s, counter + 1)) (MI.empty, counter) in
        let typ = Type_sub.sub_type s typ in
        (MI.empty, Type_sub.sub_type s typ, counter, MI.singleton nid typ, MI.empty)
    | Op (nid, e1, op, e2) ->
        let (s1, t1, counter, ets1, idents1) = pt tenv e1 counter in
        let (s2, t2, counter, ets2, idents2) = pt tenv e2 counter in
        let s3 = unify ((t1, Type.Int) :: (t2, Type.Int) :: List.concat [from_sub s1; from_sub s2]) in
        let t3 = (match op with Lt -> Type.Bool | _ -> Type.Int) in
        let typ = Type_sub.sub_type s3 t3 in
        (s3, typ, counter, ets1 |> MI.add_seq (MI.to_seq ets2) |> MI.add nid typ, idents1 |> MI.add_seq (MI.to_seq idents2))
    | If (nid, e1, e2, e3) ->
        let (s1, t1, counter, ets1, idents1) = pt tenv e1 counter in
        let (s2, t2, counter, ets2, idents2) = pt tenv e2 counter in
        let (s3, t3, counter, ets3, idents3) = pt tenv e3 counter in
        let s4 = unify ((t1, Type.Bool) :: (t2, t3) :: List.concat [from_sub s1; from_sub s2; from_sub s3]) in
        let typ = Type_sub.sub_type s4 t2 in
        (s4, typ, counter, ets1 |> MI.add_seq (MI.to_seq ets2) |> MI.add_seq (MI.to_seq ets3) |> MI.add nid typ, idents1 |> MI.add_seq (MI.to_seq idents2) |> MI.add_seq (MI.to_seq idents3))
    | Let (nid, id, e1, e2) ->
        let (s1, t1, counter, ets1, idents1) = pt tenv e1 counter in
        let t1c = closure t1 (Type_sub.sub_env s1 tenv) in
        let (s2, t2, counter, ets2, idents2) = pt (MI.add id t1c tenv) e2 counter in
        let s3 = unify (List.concat [from_sub s1; from_sub s2]) in
        let typ = Type_sub.sub_type s3 t2 in
        (s3, typ, counter, ets1 |> MI.add_seq (MI.to_seq ets2) |> MI.add nid typ, idents1 |> MI.add_seq (MI.to_seq idents2) |> MI.add id t1c)
    | Fun (nid, id, e) ->
        let (t1, counter) = (Type.TypeVar counter, counter + 1) in
        let (s, t2, counter, ets1, idents1) = pt (MI.add id (SI.empty, t1) tenv) e counter in
        let typ =  Type.Func(Type_sub.sub_type s t1, t2) in
        (s, typ, counter, ets1 |> MI.add nid typ, idents1 |> MI.add id (SI.empty, t1))
    | Ap (nid, e1, e2) ->
        let (s1, t1, counter, ets1, idents1) = pt tenv e1 counter in
        let (s2, t2, counter, ets2, idents2) = pt tenv e2 counter in
        let (t3, counter) = (Type.TypeVar counter, counter + 1) in
        let s3 = unify ((t1, Type.Func(t2, t3)) :: List.concat [from_sub s1; from_sub s2]) in
        let typ = Type_sub.sub_type s3 t3 in
        (s3, typ, counter, ets1 |> MI.add_seq (MI.to_seq ets2) |> MI.add nid typ, idents1 |> MI.add_seq (MI.to_seq idents2))
    | LetRecFun (nid, id1, id2, e1, e2) ->
        let (t_id1, counter) = (Type.TypeVar counter, counter + 1) in
        let (t_id2, counter) = (Type.TypeVar counter, counter + 1) in
        let (s1, t1, counter, ets1, idents1) = pt (tenv |> MI.add id1 (SI.empty, t_id1) |> MI.add id2 (SI.empty, t_id2)) e1 counter in
        let s2 = unify ((t_id1, Type.Func(t_id2, t1)) :: (from_sub s1)) in
        let t_id1c = closure (Type_sub.sub_type s2 t_id1) (Type_sub.sub_env s2 tenv) in
        let (s3, t3, counter, ets2, idents2) = pt (MI.add id1 t_id1c tenv) e2 counter in
        let s4 = unify (List.concat [from_sub s2; from_sub s3]) in
        let typ = Type_sub.sub_type s4 t3 in
        (s4, typ, counter, ets1 |> MI.add_seq (MI.to_seq ets2) |> MI.add nid typ, idents1 |> MI.add_seq (MI.to_seq idents2) |> MI.add id1 t_id1c |> MI.add id2 (SI.empty, t_id2))
    | EmptyList nid ->
        let (t1, counter) = (Type.TypeVar counter, counter + 1) in
        let typ = Type.List t1 in
        (MI.empty, Type.List t1, counter, MI.singleton nid typ, MI.empty)
    | Cons(nid, e1, e2) ->
        let (s1, t1, counter, ets1, idents1) = pt tenv e1 counter in
        let (s2, t2, counter, ets2, idents2) = pt tenv e2 counter in
        let s3 = unify ((t2, Type.List t1) :: List.concat [from_sub s1; from_sub s2]) in
        let typ = Type_sub.sub_type s3 t2 in
        (s3, typ, counter, ets1 |> MI.add_seq (MI.to_seq ets2) |> MI.add nid typ, idents1 |> MI.add_seq (MI.to_seq idents2))
    | Match (nid, e1, clauses) ->
        let (s1, t1, counter, ets1, idents1) = pt tenv e1 counter in
        let (t2, counter) = (Type.TypeVar counter, counter + 1) in
        let (s2, counter, ets2, idents2) = List.fold_left (fun (s2, counter, ets1, idents1) (pat, e2) ->
            let (pat_s, pat_t, pat_tenv, counter) = pt_pat_helper pat counter in
            let (e2_s, e2_t, counter, ets2, idents2) = pt (MI.add_seq (MI.to_seq pat_tenv) tenv) e2 counter in
            let s2 = unify ((t1, pat_t) :: (t2, e2_t) :: List.concat [from_sub pat_s; from_sub e2_s; from_sub s2]) in
            (s2, counter, ets1 |> MI.add_seq (MI.to_seq ets2), idents1 |> MI.add_seq (MI.to_seq idents2) |> MI.add_seq (MI.to_seq pat_tenv))
        ) (MI.empty, counter, MI.empty, MI.empty) clauses in
        let s3 = unify (List.concat [from_sub s1; from_sub s2]) in
        let typ = Type_sub.sub_type s3 t2 in
        (s3, typ, counter, ets1 |> MI.add_seq (MI.to_seq ets2) |> MI.add nid typ, idents1 |> MI.add_seq (MI.to_seq idents2))
