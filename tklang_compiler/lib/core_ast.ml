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
    and ('id, 'a) exprPayload =
      | Literal of literal
      | Var of 'id
      | Let of 'id * ('id, 'a) expr * ('id, 'a) expr
      | Fun of 'id * ('id, 'a) expr
      | Ap of ('id, 'a) expr * ('id, 'a) expr
      | LetRec of 'id *  ('id, 'a) expr * ('id, 'a) expr
      | Match of ('id, 'a) expr * ('id pat * ('id, 'a) expr) list
      | ApConstruct of string * ('id, 'a) constructParams
    and ('id, 'a) expr = {
      payload: ('id, 'a) exprPayload;
      meta: 'a
    }
    ;;
