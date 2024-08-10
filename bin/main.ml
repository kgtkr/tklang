open Tklang_compiler

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
    let (expr, _) = Expr.assign_id expr Expr_id_gen.make in
    let (expr, _) = Expr.ident_to_unique_id expr Var_id_gen.make in
    let (s, _, _, ets, idents) = Type_equs.pt Type_env.empty expr Type_var_id_gen.make in
    let ets = Type_sub.sub_expr_types s ets in
    let idents = Type_sub.sub_env s idents in
    let out = Expr.to_string ets idents expr in
    print_string out; print_newline ()

