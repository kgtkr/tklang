open Map_ext

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

let rec pat_to_string (idents: Type_scheme.t MI.t) (x: int pat): string =
    match x with
        | AnyPat(id) -> "{" ^ Int.to_string id ^ ":" ^ (MI.find id idents |> Type_scheme.to_string) ^ "}"
        | EmptyListPat -> "[]"
        | ConsPat(p1, p2) -> "(" ^ pat_to_string idents p1 ^ ")::(" ^ pat_to_string idents p2 ^ ")"
        | IgnorePat -> "_"

let rec to_string (ets: Type.t MI.t) (idents: Type_scheme.t MI.t) (x: (int, int) t): string =
    match x with
        | Int(nid, i) -> Int.to_string i ^ ":" ^ (MI.find nid ets |> Type.to_string)
        | Bool(nid, b) -> Bool.to_string b ^ ":" ^ (MI.find nid ets |> Type.to_string)
        | Var(nid, id) -> "{" ^ Int.to_string id ^ "}:" ^ (MI.find nid ets |> Type.to_string)
        | Op(nid, e1, op, e2) -> "(" ^ to_string ets idents e1 ^ ")" ^ op_to_string op ^ "(" ^ to_string ets idents e2 ^ "):" ^ (MI.find nid ets |> Type.to_string)
        | If(nid, e1, e2, e3) -> "if(" ^ to_string ets idents e1 ^ ")then(" ^ to_string ets idents e2 ^ ")else(" ^ to_string ets idents e3 ^ "):" ^ (MI.find nid ets |> Type.to_string)
        | Let(nid, id, e1, e2) -> "let{" ^ Int.to_string id ^ ":" ^ (MI.find id idents |> Type_scheme.to_string) ^ "}=(" ^ to_string ets idents e1 ^ ")in(" ^ to_string ets idents e2 ^ "):" ^ (MI.find nid ets |> Type.to_string)
        | Fun(nid, id, e) -> "fun{" ^ Int.to_string id ^ ":" ^ (MI.find id idents |> Type_scheme.to_string) ^ "}->(" ^ to_string ets idents e ^ "):" ^ (MI.find nid ets |> Type.to_string)
        | Ap(nid, e1, e2) -> "(" ^ to_string ets idents e1 ^ ")(" ^ to_string ets idents e2 ^ "):" ^ (MI.find nid ets |> Type.to_string)
        | LetRecFun(nid, id1, id2, e1, e2) -> "let rec{" ^ Int.to_string id1 ^ ":" ^ (MI.find id1 idents |> Type_scheme.to_string) ^ "}=fun{" ^ Int.to_string id2 ^ ":" ^ (MI.find id2 idents |> Type_scheme.to_string) ^ "}->(" ^ to_string ets idents e1 ^ ")in(" ^ to_string ets idents e2 ^ "):" ^ (MI.find nid ets |> Type.to_string)
        | EmptyList nid -> "[]:" ^ (MI.find nid ets |> Type.to_string)
        | Cons(nid, e1, e2) -> "(" ^ to_string ets idents e1 ^ ")::(" ^ to_string ets idents e2 ^ "):" ^ (MI.find nid ets |> Type.to_string)
        | Match(nid, e1, clauses) -> "match(" ^ to_string ets idents e1 ^ ")with" ^ (clauses |> List.map (fun (pat, e) -> "|" ^ pat_to_string idents pat ^ "->" ^ "(" ^ to_string ets idents e ^ ")") |> String.concat "") ^ ":" ^ (MI.find nid ets |> Type.to_string)

let rec id_string_to_int_pat (pat: string pat) (counter: int): (int pat * int MS.t * string MI.t * int) =
    match pat with
    | AnyPat id ->
        let (id, env, s, counter) = (counter, MS.singleton id counter, MI.singleton counter id, counter + 1) in
        (AnyPat id, env, s, counter)
    | EmptyListPat ->
        (EmptyListPat, MS.empty, MI.empty, counter)
    | ConsPat(pat1, pat2) ->
        let (pat1, env1, s1, counter) = id_string_to_int_pat pat1 counter in
        let (pat2, env2, s2, counter) = id_string_to_int_pat pat2 counter in
        (ConsPat(pat1, pat2), env1 |> MS.add_seq (MS.to_seq env2), s1 |> MI.add_seq (MI.to_seq s2), counter)
    | IgnorePat ->
        (IgnorePat, MS.empty, MI.empty, counter)

let rec id_string_to_int (env: int MS.t) (e: (string, 'a) t) (counter: int): ((int, 'a) t * string MI.t * int) =
    match e with
        | Int (m, a) -> (Int (m, a), MI.empty, counter)
        | Bool (m, a) -> (Bool (m, a), MI.empty, counter)
        | Var (m, id) -> (Var (m, MS.find id env), MI.empty, counter)
        | Op (m, e1, a, e2) ->
            let (e1, s1, counter) = id_string_to_int env e1 counter in
            let (e2, s2, counter) = id_string_to_int env e2 counter in
            (Op (m, e1, a, e2), MI.add_seq (MI.to_seq s2) s1, counter)
        | If (m, e1, e2, e3) ->
            let (e1, s1, counter) = id_string_to_int env e1 counter in
            let (e2, s2, counter) = id_string_to_int env e2 counter in
            let (e3, s3, counter) = id_string_to_int env e3 counter in
            (If (m, e1, e2, e3), s1 |> MI.add_seq (MI.to_seq s2) |>  MI.add_seq (MI.to_seq s3), counter)
        | Let (m, id, e1, e2) ->
            let (e1, s1, counter) = id_string_to_int env e1 counter in
            let (id, env2, s2, counter) = (counter, MS.add id counter env, MI.singleton counter id, counter + 1) in
            let (e2, s3, counter) = id_string_to_int env2 e2 counter in
            (Let (m, id, e1, e2), s1 |> MI.add_seq (MI.to_seq s2) |>  MI.add_seq (MI.to_seq s3), counter)
        | Fun (m, id, e) ->
            let (id, env2, s1, counter) = (counter, MS.add id counter env, MI.singleton counter id, counter + 1) in
            let (e, s2, counter) = id_string_to_int env2 e counter in
            (Fun (m, id, e), s1 |> MI.add_seq (MI.to_seq s2), counter)
        | Ap (m, e1, e2) ->
            let (e1, s1, counter) = id_string_to_int env e1 counter in
            let (e2, s2, counter) = id_string_to_int env e2 counter in
            (Ap (m, e1, e2), s1 |> MI.add_seq (MI.to_seq s2), counter)
        | LetRecFun (m, id, id2, e1, e2) ->
            let (id, env2, s1, counter) = (counter, MS.add id counter env, MI.singleton counter id, counter + 1) in
            let (id2, env3, s2, counter) = (counter, MS.add id2 counter env2, MI.singleton counter id2, counter + 1) in
            let (e1, s3, counter) = id_string_to_int env3 e1 counter in
            let (e2, s4, counter) = id_string_to_int env2 e2 counter in
            (LetRecFun (m, id, id2, e1, e2), s1 |> MI.add_seq (MI.to_seq s2) |>  MI.add_seq (MI.to_seq s3) |>  MI.add_seq (MI.to_seq s4), counter)
        | EmptyList (m) -> (EmptyList (m), MI.empty, counter)
        | Cons (m, e1, e2) ->
            let (e1, s1, counter) = id_string_to_int env e1 counter in
            let (e2, s2, counter) = id_string_to_int env e2 counter in
            (Cons (m, e1, e2), s1 |> MI.add_seq (MI.to_seq s2), counter)
        | Match (m, e1, clauses) ->
            let (e1, s1, counter) = id_string_to_int env e1 counter in
            let (clauses, s2, counter) =
                List.fold_right (fun (pat, e2) (clauses, s2, counter) ->
                    let (pat, env2, s3, counter) = id_string_to_int_pat pat counter in
                    let (e2, s4, counter) = id_string_to_int (env |> MS.add_seq (MS.to_seq env2)) e2 counter in
                    ((pat, e2) :: clauses, s2 |> MI.add_seq (MI.to_seq s3) |> MI.add_seq (MI.to_seq s4), counter)
                ) clauses ([], MI.empty, counter) in
            (Match (m, e1, clauses), s1 |> MI.add_seq (MI.to_seq s2), counter)


let rec assign_id (e: ('id, 'a) t) (counter: int): (('id, int) t * int) =
    match e with
        | Int (_, a) -> (Int (counter, a), counter + 1)
        | Bool (_, a) -> (Bool (counter, a), counter + 1)
        | Var (_, a) -> (Var (counter, a), counter + 1)
        | Op (_, e1, a, e2) ->
            let (e1, counter) = assign_id e1 counter in
            let (e2, counter) = assign_id e2 counter in
            (Op (counter, e1, a, e2), counter + 1)
        | If (_, e1, e2, e3) ->
            let (e1, counter) = assign_id e1 counter in
            let (e2, counter) = assign_id e2 counter in
            let (e3, counter) = assign_id e3 counter in
            (If (counter, e1, e2, e3), counter + 1)
        | Let (_, a, e1, e2) ->
            let (e1, counter) = assign_id e1 counter in
            let (e2, counter) = assign_id e2 counter in
            (Let (counter, a, e1, e2), counter + 1)
        | Fun (_, a, e1) ->
            let (e1, counter) = assign_id e1 counter in
            (Fun (counter, a, e1), counter + 1)
        | Ap (_, e1, e2) ->
            let (e1, counter) = assign_id e1 counter in
            let (e2, counter) = assign_id e2 counter in
            (Ap (counter, e1, e2), counter + 1)
        | LetRecFun (_, a, b, e1, e2) ->
            let (e1, counter) = assign_id e1 counter in
            let (e2, counter) = assign_id e2 counter in
            (LetRecFun (counter, a, b, e1, e2), counter + 1)
        | EmptyList (_) -> (EmptyList counter, counter + 1)
        | Cons (_, e1, e2) ->
            let (e1, counter) = assign_id e1 counter in
            let (e2, counter) = assign_id e2 counter in
            (Cons (counter, e1, e2), counter + 1)
        | Match (_, e1, clauses) ->
            let (e1, counter) = assign_id e1 counter in
            let (clauses, counter) = List.fold_right (fun (pat, e2) (clauses, counter) ->
                let (e2, counter) = assign_id e2 counter in
                ((pat, e2) :: clauses, counter)
            ) clauses ([], counter) in
            (Match (counter, e1, clauses), counter + 1)
