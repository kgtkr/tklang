type 'a parser = Token.t list -> ('a * Token.t list) option
type expr = (string, unit) Expr.t
type clause = (string, unit) Expr.clause
type pat = string Expr.pat

let bind (f: 'a -> 'b parser) (p: 'a parser): 'b parser = fun xs ->
    match p xs with
    | Some (x, xs) -> f x xs
    | None -> None

let pure (x: 'a): 'a parser = fun xs -> Some (x, xs)

let ( let* ) x f = bind f x

let map (ab: 'a -> 'b) (ap: 'a parser): 'b parser = 
    let* a = ap in
    pure (ab a)

let many (p: 'a parser): ('a list) parser =
    let rec many_helper (acc: 'a list): ('a list) parser = fun xs ->
        match p xs with
        | Some (x, xs) -> many_helper (x :: acc) xs
        | None -> Some (acc, xs)
    in
        many_helper [] |> map List.rev

let tuple (ap: 'a parser) (bp: 'b parser): ('a * 'b) parser =
    let* a = ap in
    let* b = bp in
    pure (a, b)

let left_join (p: 'a parser) (op: ('a -> 'a -> 'a) parser): 'a parser =
    let* x = p in
    let* ys = many (tuple op p) in
    pure (List.fold_left (fun x (op, y) -> op x y) x ys)

let right_join (p: 'a parser) (op: ('a -> 'a -> 'a) parser): 'a parser =
    let* x = p in
    let* ys = many (tuple op p) in
    pure (List.fold_right (fun (op, y) k x -> op x (k y)) ys Fun.id x)

let or_ (p: 'a parser) (q: 'a parser): 'a parser = fun xs ->
    match p xs with
    | Some (x, xs) -> Some (x, xs)
    | None -> q xs

let choice (ps: ('a parser) list): 'a parser =
    List.fold_left or_ (Fun.const None) ps

let satisfy (f: Token.t -> 'a option): 'a parser = fun xs ->
    match xs with
    | [] -> None
    | (x :: xs) -> (match f x with
                        | Some y -> Some (y, xs)
                        | None -> None)

let rec
    atom_parser: expr parser = fun xs ->
        match xs with
        | Token.Int n :: xs -> Some (Expr.Int((), n), xs)
        | Token.ReservedKeyword Token.True :: xs -> Some (Expr.Bool((), true), xs)
        | Token.ReservedKeyword Token.False :: xs -> Some (Expr.Bool((), false), xs)
        | Token.Ident x :: xs -> Some (Expr.Var((), x), xs)
        | Token.BracketBegin :: xs -> (match xs with | Token.BracketEnd :: xs -> Some (Expr.EmptyList (), xs) | _ -> None)
        | Token.ParentBegin :: xs -> (
                match expr_parser xs with
                | Some (expr, xs) -> (match xs with | Token.ParentEnd :: xs -> Some (expr, xs) | _ -> None)
                | None -> None
            )
        | _ -> None
    and ap_parser: expr parser = fun xs -> left_join atom_parser (pure (fun a b -> Expr.Ap ((), a, b))) xs
    and term_parser: expr parser = fun xs -> left_join ap_parser (satisfy (fun x -> match x with | Token.Op "*" -> Some (fun a b -> Expr.Op ((), a, Expr.Mul, b)) | _ -> None)) xs
    and addsub_parser: expr parser = fun xs -> left_join term_parser (satisfy (fun x -> match x with | Token.Op "+" -> Some (fun a b -> Expr.Op ((), a, Expr.Add, b)) | Token.Op "-" -> Some (fun a b -> Expr.Op ((), a, Expr.Sub, b)) | _ -> None)) xs
    and cons_parser: expr parser = fun xs -> right_join addsub_parser (satisfy (fun x -> match x with | Token.Op "::" -> Some (fun a b -> Expr.Cons ((), a, b)) | _ -> None)) xs
    and if_parser: expr parser = fun xs ->
        (
            let* _ = satisfy (fun x -> match x with | Token.ReservedKeyword Token.If -> Some (()) | _ -> None) in
            let* e1 = expr_parser in
            let* _ = satisfy (fun x -> match x with | Token.ReservedKeyword Token.Then -> Some (()) | _ -> None) in
            let* e2 = expr_parser in
            let* _ = satisfy (fun x -> match x with | Token.ReservedKeyword Token.Else -> Some (()) | _ -> None) in
            let* e3 = expr_parser in
            pure (Expr.If ((), e1, e2, e3))
        ) xs
    and let_parser: expr parser = fun xs ->
        (
            let* _ = satisfy (fun x -> match x with | Token.ReservedKeyword Token.Let -> Some (()) | _ -> None) in
            let* id = satisfy (fun x -> match x with | Token.Ident id -> Some (id) | _ -> None) in
            let* _ = satisfy (fun x -> match x with | Token.ReservedSymbol Token.Equal -> Some (()) | _ -> None) in
            let* e1 = expr_parser in
            let* _ = satisfy (fun x -> match x with | Token.ReservedKeyword Token.In -> Some (()) | _ -> None) in
            let* e2 = expr_parser in
            pure (Expr.Let ((), id, e1, e2))
        ) xs
    and fun_parser: expr parser = fun xs ->
        (
            let* _ = satisfy (fun x -> match x with | Token.ReservedKeyword Token.Fun -> Some (()) | _ -> None) in
            let* id = satisfy (fun x -> match x with | Token.Ident id -> Some (id) | _ -> None) in
            let* _ = satisfy (fun x -> match x with | Token.ReservedSymbol Token.RArrow -> Some (()) | _ -> None) in
            let* e = expr_parser in
            pure (Expr.Fun ((), id, e))
        ) xs
    and let_rec_fun_parser: expr parser = fun xs ->
        (
            let* _ = satisfy (fun x -> match x with | Token.ReservedKeyword Token.Let -> Some (()) | _ -> None) in
            let* _ = satisfy (fun x -> match x with | Token.ReservedKeyword Token.Rec -> Some (()) | _ -> None) in
            let* id1 = satisfy (fun x -> match x with | Token.Ident id -> Some (id) | _ -> None) in
            let* _ = satisfy (fun x -> match x with | Token.ReservedSymbol Token.Equal -> Some (()) | _ -> None) in
            let* _ = satisfy (fun x -> match x with | Token.ReservedKeyword Token.Fun -> Some (()) | _ -> None) in
            let* id2 = satisfy (fun x -> match x with | Token.Ident id -> Some (id) | _ -> None) in
            let* _ = satisfy (fun x -> match x with | Token.ReservedSymbol Token.RArrow -> Some (()) | _ -> None) in
            let* e1 = expr_parser in
            let* _ = satisfy (fun x -> match x with | Token.ReservedKeyword Token.In -> Some (()) | _ -> None) in
            let* e2 = expr_parser in
            pure (Expr.LetRecFun ((), id1, id2, e1, e2))
        ) xs
    and atom_pat_parser: pat parser = fun xs ->
        match xs with
        | Token.Ident x :: xs -> Some (Expr.AnyPat x, xs)
        | Token.Underscore :: xs -> Some (Expr.IgnorePat, xs)
        | Token.BracketBegin :: xs -> (match xs with | Token.BracketEnd :: xs -> Some (Expr.EmptyListPat, xs) | _ -> None)
        | _ -> None
    and pat_parser: pat parser = fun xs -> right_join atom_pat_parser (satisfy (fun x -> match x with | Token.Op "::" -> Some (fun a b -> Expr.ConsPat (a, b)) | _ -> None)) xs
    and clause_parser: clause parser = fun xs ->
        (
            let* _ = satisfy (fun x -> match x with | Token.ReservedSymbol Token.VerticalBar -> Some (()) | _ -> None) in
            let* pat = pat_parser in
            let* _ = satisfy (fun x -> match x with | Token.ReservedSymbol Token.RArrow -> Some (()) | _ -> None) in
            let* e = expr_parser in
            pure (pat, e)
        ) xs
    and match_parser: expr parser = fun xs ->
        (
            let* _ = satisfy (fun x -> match x with | Token.ReservedKeyword Token.Match -> Some (()) | _ -> None) in
            let* e1 = expr_parser in
            let* _ = satisfy (fun x -> match x with | Token.ReservedKeyword Token.With -> Some (()) | _ -> None) in
            let* clauses = many clause_parser in
            pure (Expr.Match ((), e1, clauses))
        ) xs
    and expr_parser: expr parser = fun xs ->
        choice [
            if_parser;
            let_parser;
            fun_parser;
            let_rec_fun_parser;
            match_parser;
            cons_parser
        ] xs
        
