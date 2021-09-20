open Tklang_compiler
open Tklang_compiler.Map_ext

exception Exception

(* https://enakai00.hatenablog.com/entry/20110419/1303191953 *)
let read_all () =
  let rec read_all_sub result =
    try read_all_sub ( read_line () :: result )
    with End_of_file -> ( List.rev result |> String.concat "\n" )
  in read_all_sub []

let () =
    let src = read_all () in
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
    print_string out; print_newline ()

