exception LexError

let is_ident_first (c: char): bool = 'a' <= c && c <= 'z'

let is_ident_rest (c: char): bool = is_ident_first c || c == '_' || 'A' <= c && c <= 'Z' || '0' <= c && c <= '9'

let is_digit (c: char): bool = '0' <= c && c <= '9'

let is_symbol (c: char): bool = c == '+' || c == '-' || c == '*' || c == ':' || c == '>' || c == '<' || c == '=' || c == '|'

let rec split_while_helper (f: ('a) -> bool) (a: 'a list) (b: 'a list): ('a list * 'a list) =
    match a with
    | [] -> (a, b)
    | x :: xs -> if f x then split_while_helper f xs (x :: b) else (a, b)

let split_while (f: ('a) -> bool) (xs: 'a list): ('a list * 'a list) =
    let (ys, zs) = split_while_helper f xs [] in (List.rev zs, ys)

(* -は単項演算子として実装するので負のリテラルは無視 *)
let rec lex_helper (input: char list) (tokens: Token.t list): Token.t list =
    match input with
        | [] -> tokens
        | (' ' | '\n' | '\t' | '\r') :: cs -> lex_helper cs tokens
        | '('::cs -> lex_helper cs (Token.ParentBegin :: tokens)
        | ')'::cs -> lex_helper cs (Token.ParentEnd :: tokens)
        | '['::cs -> lex_helper cs  (Token.BracketBegin :: tokens)
        | ']'::cs -> lex_helper cs (Token.BracketEnd :: tokens)
        | '_'::cs -> lex_helper cs (Token.Underscore :: tokens)
        | c::cs when is_digit(c) ->
            let (ds, cs) = split_while is_digit cs in
            let n = (c :: ds) |> List.to_seq |> String.of_seq |> int_of_string in
            lex_helper cs (Token.Int n :: tokens)
        | c::cs when is_ident_first(c) ->
            let (ident_rest, cs) = split_while is_ident_rest cs in
            let ident = (c :: ident_rest) |> List.to_seq |> String.of_seq in
            lex_helper cs ((match ident with
            | "if" -> Token.ReservedKeyword Token.If
            | "then" -> Token.ReservedKeyword Token.Then
            | "else" -> Token.ReservedKeyword Token.Else
            | "let" -> Token.ReservedKeyword Token.Let
            | "in" -> Token.ReservedKeyword Token.In
            | "fun" -> Token.ReservedKeyword Token.Fun
            | "rec" -> Token.ReservedKeyword Token.Rec
            | "match" -> Token.ReservedKeyword Token.Match
            | "with" -> Token.ReservedKeyword Token.With
            | "true" -> Token.ReservedKeyword Token.True
            | "false" -> Token.ReservedKeyword Token.False
            | ident -> Token.Ident ident) :: tokens)
        | c::cs when is_symbol(c) ->
            let (symbol_rest, cs) = split_while is_symbol cs in
            let symbol = (c :: symbol_rest) |> List.to_seq |> String.of_seq in
            lex_helper cs ((match symbol with
            | "->" -> Token.ReservedSymbol Token.RArrow
            | "|" -> Token.ReservedSymbol Token.VerticalBar
            | "=" -> Token.ReservedSymbol Token.Equal
            | symbol -> Token.Op symbol) :: tokens)
        | _ -> raise LexError

let lex (input: char list): Token.t list =
    List.rev (lex_helper input [])
