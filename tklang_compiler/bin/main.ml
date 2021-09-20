open Tklang_compiler
open Tklang_compiler.Map_ext

exception Exception

let () =
    let src = really_input_string stdin (in_channel_length stdin) in
    let tokens = Lexer.lex (src |> String.to_seq |> List.of_seq) in
    let expr = match Parser.expr_parser tokens with
        | Some (ast, []) -> ast
        | _ -> raise Exception in
    let (expr, _) = Expr.assign_id expr 0 in
    let (expr, _, _) = Expr.id_string_to_int MS.empty expr 0 in
    let (s, _, _, ets, idents) = Type_equ.pt MI.empty expr 0 in
    let ets = MI.map (Type_sub.sub_type s) ets in
    let idents = MI.map (Type_sub.sub_type_scheme s) idents in
    let out = Expr.to_string ets idents expr in
    print_string out

