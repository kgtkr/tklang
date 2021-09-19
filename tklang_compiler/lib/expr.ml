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
