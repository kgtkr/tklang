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
