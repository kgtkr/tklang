type 'a parser = Token.t list -> ('a * Token.t list) option
type expr = (string, unit) Expr.t


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
        | Token.Ident x :: xs -> Some (Expr.Var((), x), xs)
        | Token.ParentBegin :: xs -> (
                match expr_parser xs with
                | Some (expr, xs) -> (match xs with | Token.ParentEnd :: xs -> Some (expr, xs) | _ -> None)
                | None -> None
            )
        | _ -> None
    and ap_parser: expr parser = fun xs -> left_join atom_parser (pure (fun a b -> Expr.Ap ((), a, b))) xs
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
    and expr_parser: expr parser = fun xs ->
        choice [
            ap_parser;
            atom_parser;
            let_parser;
            fun_parser;
        ] xs
        
