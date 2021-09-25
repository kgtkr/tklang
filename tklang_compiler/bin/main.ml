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
    let tokens = Lexer.lex src in
    let expr = match Parser.parse tokens with
        | Some expr -> expr
        | _ -> raise Exception in
    let (expr, _) = Expr.assign_id expr Expr_id_gen.make in
    let (expr, _) = Expr.ident_to_unique_id expr Var_id_gen.make in
    let (ets, tenv_acc) = Type_equs.pt expr in
    let out = Expr.to_string ets tenv_acc expr in
    print_string out; print_newline ()

