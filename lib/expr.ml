type ('id, 'a) t =
    | Int of 'a * int
    | Var of 'a * 'id
    | Let of 'a * 'id * ('id, 'a) t * ('id, 'a) t
    | Fun of 'a * 'id * ('id, 'a) t
    | Ap of 'a * ('id, 'a) t * ('id, 'a) t;;


let rec to_string (ets: Expr_types.t) (idents: Type_env.t) (x: (Var_id.t, Expr_id.t) t): string =
    match x with
        | Int(nid, i) -> Int.to_string i ^ ": " ^ (Expr_types.get_exn nid ets |> Type.to_string)
        | Var(nid, id) -> Var_id.to_string id ^ ": " ^ (Expr_types.get_exn nid ets |> Type.to_string)
        | Let(nid, id, e1, e2) -> "(let " ^ Var_id.to_string_with_type_scheme id (Type_env.get_exn id idents) ^ " = " ^ to_string ets idents e1 ^ " in " ^ to_string ets idents e2 ^ "): " ^ (Expr_types.get_exn nid ets |> Type.to_string)
        | Fun(nid, id, e) -> "(fun " ^ Var_id.to_string_with_type_scheme id (Type_env.get_exn id idents) ^ " -> " ^ to_string ets idents e ^ "): " ^ (Expr_types.get_exn nid ets |> Type.to_string)
        | Ap(nid, e1, e2) -> "(" ^ to_string ets idents e1 ^ " " ^ to_string ets idents e2 ^ "): " ^ (Expr_types.get_exn nid ets |> Type.to_string)

let rec ident_to_unique_id (e: (string, 'a) t) (gen: Var_id_gen.t): ((Var_id.t, 'a) t * Var_id_gen.t) =
    match e with
        | Int (m, a) -> (Int (m, a), gen)
        | Var (m, ident) -> (Var (m, Var_id_gen.lookup_exn ident gen), gen)
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


let rec assign_id (e: ('id, 'a) t) (gen: Expr_id_gen.t): (('id, Expr_id.t) t * Expr_id_gen.t) =
    let (id, gen) = Expr_id_gen.gen gen in 
    match e with
    | Int (_, a) ->
            (Int (id, a), gen)
        | Var (_, a) ->
            (Var (id, a), gen)
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
