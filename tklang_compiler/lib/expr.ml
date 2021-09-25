type
    op = Add | Sub | Mul | Lt
    and 'id pat = AnyPat of 'id | EmptyListPat | ConsPat of 'id pat * 'id pat | IgnorePat
    and ('id, 'a) clause = 'id pat * ('id, 'a) t
    and ('id, 'a) t =
    | Int of 'a * int
    | Bool of 'a * bool
    | Var of 'a * 'id
    | Op of 'a * ('id, 'a) t * op * ('id, 'a) t
    | If of 'a * ('id, 'a) t * ('id, 'a) t * ('id, 'a) t
    | Let of 'a * 'id * ('id, 'a) t * ('id, 'a) t
    | Fun of 'a * 'id * ('id, 'a) t
    | Ap of 'a * ('id, 'a) t * ('id, 'a) t
    | LetRecFun of 'a * 'id * 'id * ('id, 'a) t * ('id, 'a) t
    | EmptyList of 'a
    | Cons of 'a * ('id, 'a) t * ('id, 'a) t
    | Match of 'a * ('id, 'a) t * ('id, 'a) clause list;;

let op_to_string (op: op): string =
    match op with
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Lt -> "<"

let rec pat_to_string (tenv: Type_env.t) (x: Var_id.t pat): string =
    match x with
        | AnyPat(id) -> Var_id.to_string_with_type_scheme id (Type_env.get_exn id tenv)
        | EmptyListPat -> "[]"
        | ConsPat(p1, p2) -> "(" ^ pat_to_string tenv p1 ^ "::" ^ pat_to_string tenv p2 ^ ")"
        | IgnorePat -> "_"

let rec to_string (ets: Expr_types.t) (tenv_acc: Type_env.t) (x: (Var_id.t, Expr_id.t) t): string =
    match x with
        | Int(nid, i) -> Int.to_string i ^ ": " ^ (Expr_types.get_exn nid ets |> Type.to_string)
        | Bool(nid, b) -> Bool.to_string b ^ ": " ^ (Expr_types.get_exn nid ets |> Type.to_string)
        | Var(nid, id) -> Var_id.to_string id ^ ": " ^ (Expr_types.get_exn nid ets |> Type.to_string)
        | Op(nid, e1, op, e2) -> "(" ^ to_string ets tenv_acc e1 ^ op_to_string op ^ to_string ets tenv_acc e2 ^ "): " ^ (Expr_types.get_exn nid ets |> Type.to_string)
        | If(nid, e1, e2, e3) -> "(if " ^ to_string ets tenv_acc e1 ^ " then " ^ to_string ets tenv_acc e2 ^ " else " ^ to_string ets tenv_acc e3 ^ "): " ^ (Expr_types.get_exn nid ets |> Type.to_string)
        | Let(nid, id, e1, e2) -> "(let " ^ Var_id.to_string_with_type_scheme id (Type_env.get_exn id tenv_acc) ^ " = " ^ to_string ets tenv_acc e1 ^ " in " ^ to_string ets tenv_acc e2 ^ "): " ^ (Expr_types.get_exn nid ets |> Type.to_string)
        | Fun(nid, id, e) -> "(fun " ^ Var_id.to_string_with_type_scheme id (Type_env.get_exn id tenv_acc) ^ " -> " ^ to_string ets tenv_acc e ^ "): " ^ (Expr_types.get_exn nid ets |> Type.to_string)
        | Ap(nid, e1, e2) -> "(" ^ to_string ets tenv_acc e1 ^ " " ^ to_string ets tenv_acc e2 ^ "): " ^ (Expr_types.get_exn nid ets |> Type.to_string)
        | LetRecFun(nid, id1, id2, e1, e2) -> "(let rec " ^ Var_id.to_string_with_type_scheme id1 (Type_env.get_exn id1 tenv_acc) ^ " = fun " ^ Var_id.to_string_with_type_scheme id2 (Type_env.get_exn id2 tenv_acc) ^ " -> " ^ to_string ets tenv_acc e1 ^ " in " ^ to_string ets tenv_acc e2 ^ "): " ^ (Expr_types.get_exn nid ets |> Type.to_string)
        | EmptyList nid -> "[]: " ^ (Expr_types.get_exn nid ets |> Type.to_string)
        | Cons(nid, e1, e2) -> "(" ^ to_string ets tenv_acc e1 ^ "::" ^ to_string ets tenv_acc e2 ^ "): " ^ (Expr_types.get_exn nid ets |> Type.to_string)
        | Match(nid, e1, clauses) -> "(match " ^ to_string ets tenv_acc e1 ^ " with " ^ (clauses |> List.map (fun (pat, e) -> " | " ^ pat_to_string tenv_acc pat ^ " -> " ^ to_string ets tenv_acc e) |> String.concat "") ^ "): " ^ (Expr_types.get_exn nid ets |> Type.to_string)

let rec ident_to_unique_id_pat (pat: string pat) (gen: Var_id_gen.t): (Var_id.t pat * Var_id_gen.t) =
    match pat with
    | AnyPat ident ->
        let (id, gen) = Var_id_gen.gen ident gen in
        (AnyPat id, gen)
    | EmptyListPat ->
        (EmptyListPat, gen)
    | ConsPat(pat1, pat2) ->
        let (pat1, gen) = ident_to_unique_id_pat pat1 gen in
        let (pat2, gen) = ident_to_unique_id_pat pat2 gen in
        (ConsPat(pat1, pat2), gen)
    | IgnorePat ->
        (IgnorePat, gen)

let rec ident_to_unique_id (e: (string, 'a) t) (gen: Var_id_gen.t): ((Var_id.t, 'a) t * Var_id_gen.t) =
    match e with
        | Int (m, a) -> (Int (m, a), gen)
        | Bool (m, a) -> (Bool (m, a), gen)
        | Var (m, ident) -> (Var (m, Var_id_gen.lookup_exn ident gen), gen)
        | Op (m, e1, a, e2) ->
            let (e1, gen) = ident_to_unique_id e1 gen in
            let (e2, gen) = ident_to_unique_id e2 gen in
            (Op (m, e1, a, e2), gen)
        | If (m, e1, e2, e3) ->
            let (e1, gen) = ident_to_unique_id e1 gen in
            let (e2, gen) = ident_to_unique_id e2 gen in
            let (e3, gen) = ident_to_unique_id e3 gen in
            (If (m, e1, e2, e3), gen)
        | Let (m, ident, e1, e2) ->
            let (e1, gen) = ident_to_unique_id e1 gen in
            let ((id, e2), gen) = gen |> Var_id_gen.scoped (
                let (id, gen) = Var_id_gen.gen ident gen in
                let (e2, gen) = ident_to_unique_id e2 gen in
                ((id, e2), gen)
            ) in
            (Let (m, id, e1, e2), gen)
        | Fun (m, ident, e) ->
            let ((id, e), gen) = gen |> Var_id_gen.scoped (
                let (id, gen) = Var_id_gen.gen ident gen in
                let (e, gen) = ident_to_unique_id e gen in
                ((id, e), gen)
            ) in
            (Fun (m, id, e), gen)
        | Ap (m, e1, e2) ->
            let (e1, gen) = ident_to_unique_id e1 gen in
            let (e2, gen) = ident_to_unique_id e2 gen in
            (Ap (m, e1, e2), gen)
        | LetRecFun (m, ident1, ident2, e1, e2) ->
            let ((id1, id2, e1, e2), gen) = gen |> Var_id_gen.scoped (
                let (id1, gen) = Var_id_gen.gen ident1 gen in
                let (id2, gen) = Var_id_gen.gen ident2 gen in
                let (e1, gen) = ident_to_unique_id e1 gen in
                let (e2, gen) = ident_to_unique_id e2 gen in
                ((id1, id2, e1, e2), gen)
            ) in
            (LetRecFun (m, id1, id2, e1, e2), gen)
        | EmptyList (m) -> (EmptyList (m), gen)
        | Cons (m, e1, e2) ->
            let (e1, gen) = ident_to_unique_id e1 gen in
            let (e2, gen) = ident_to_unique_id e2 gen in
            (Cons (m, e1, e2), gen)
        | Match (m, e1, clauses) ->
            let (e1, gen) = ident_to_unique_id e1 gen in
            let (clauses, gen) =
                List.fold_right (fun (pat, e2) (clauses, gen) ->
                    gen |> Var_id_gen.scoped (
                        let (pat, gen) = ident_to_unique_id_pat pat gen in
                        let (e2, gen) = ident_to_unique_id e2 gen in
                        ((pat, e2) :: clauses, gen)
                    )
                ) clauses ([], gen) in
            (Match (m, e1, clauses), gen)


let rec assign_id (e: ('id, 'a) t) (gen: Expr_id_gen.t): (('id, Expr_id.t) t * Expr_id_gen.t) =
    let (id, gen) = Expr_id_gen.gen gen in 
    match e with
        | Int (_, a) ->
            (Int (id, a), gen)
        | Bool (_, a) ->
            (Bool (id, a), gen)
        | Var (_, a) ->
            (Var (id, a), gen)
        | Op (_, e1, a, e2) ->
            let (e1, gen) = assign_id e1 gen in
            let (e2, gen) = assign_id e2 gen in
            (Op (id, e1, a, e2), gen)
        | If (_, e1, e2, e3) ->
            let (e1, gen) = assign_id e1 gen in
            let (e2, gen) = assign_id e2 gen in
            let (e3, gen) = assign_id e3 gen in
            (If (id, e1, e2, e3), gen)
        | Let (_, a, e1, e2) ->
            let (e1, gen) = assign_id e1 gen in
            let (e2, gen) = assign_id e2 gen in
            (Let (id, a, e1, e2), gen)
        | Fun (_, a, e1) ->
            let (e1, gen) = assign_id e1 gen in
            (Fun (id, a, e1), gen)
        | Ap (_, e1, e2) ->
            let (e1, gen) = assign_id e1 gen in
            let (e2, gen) = assign_id e2 gen in
            (Ap (id, e1, e2), gen)
        | LetRecFun (_, a, b, e1, e2) ->
            let (e1, gen) = assign_id e1 gen in
            let (e2, gen) = assign_id e2 gen in
            (LetRecFun (id, a, b, e1, e2), gen)
        | EmptyList (_) -> (EmptyList id, gen)
        | Cons (_, e1, e2) ->
            let (e1, gen) = assign_id e1 gen in
            let (e2, gen) = assign_id e2 gen in
            (Cons (id, e1, e2), gen)
        | Match (_, e1, clauses) ->
            let (e1, gen) = assign_id e1 gen in
            let (clauses, gen) = List.fold_right (fun (pat, e2) (clauses, gen) ->
                let (e2, gen) = assign_id e2 gen in
                ((pat, e2) :: clauses, gen)
            ) clauses ([], gen) in
            (Match (id, e1, clauses), gen)
