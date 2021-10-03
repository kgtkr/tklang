type
    'id constructPat =
      | UnnamedPat of ('id pat) list
      | NamedPat of (string * 'id pat) list
      | NonParamsPat
    and literalPat =
      | IntPat of int
    and 'id pat = AnyPat of 'id | ConstructPat of 'id constructPat | LiteralPat of literalPat  | IgnorePat
    and literal =
      | Int of int
    and ('id, 'a) constructParams =
      | Unnamed of (('id, 'a) expr) list
      | Named of (string * ('id, 'a) expr) list
      | NonParams
    and ('id, 'a) expr =
    | Literal of 'a * literal
    | Var of 'a * 'id
    | Let of 'a * 'id * ('id, 'a) expr * ('id, 'a) expr
    | Fun of 'a * 'id * ('id, 'a) expr
    | Ap of 'a * ('id, 'a) expr * ('id, 'a) expr
    | LetRec of 'a * 'id *  ('id, 'a) expr * ('id, 'a) expr
    | Match of 'a * ('id, 'a) expr * ('id pat * ('id, 'a) expr) list
    | ApConstruct of 'a * string * ('id, 'a) constructParams
    ;;
