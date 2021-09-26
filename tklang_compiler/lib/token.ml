type reserved_keyword  =
    | If
    | Then
    | Else
    | Let
    | In
    | Fun
    | Rec
    | Match
    | With
    | True
    | False

type reserved_symbol =
    | RArrow
    | VerticalBar
    | Equal
    
type t =
    | SymbolIdent of string
    | LowerIdent of string
    | Int of int
    | ReservedKeyword of reserved_keyword
    | ReservedSymbol of reserved_symbol
    | ParentBegin
    | ParentEnd
    | BracketBegin
    | BracketEnd
    | Underscore
