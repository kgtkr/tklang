# tklang compiler for プログラミング基礎論期末レポート用ブランチ
## 例

```
$ echo "1" | dune exec tklangc
1: int

$ echo "let x = 1 in x" | dune exec tklangc
(let <0: {}int> = 1: int in <0>: int): int

$ echo "let id = fun x -> x in id 1" | dune exec tklangc
(let <1: {'0}('0 -> '0)> = (fun <0: {}'0> -> <0>: '0): ('0 -> '0) in (<1>: (int -> int) 1: int): int): int

# id関数を多相的に使えること(intを渡してもa->intを渡しても動くこと)を確認
$ echo "let id = fun x -> x in let x1 = id 1 in let x2 = id (fun x -> 0) in 0" | dune exec tklangc
(let <1: {'0}('0 -> '0)> = (fun <0: {}'0> -> <0>: '0): ('0 -> '0) in (let <2: {}int> = (<1>: (int -> int) 1: int): int in (let <4: {'4}('4 -> int)> = (<1>: (('4 -> int) -> ('4 -> int)) (fun <3: {}'4> -> 0: int): ('4 -> int)): ('4 -> int) in 0: int): int): int): int

# letを脱等するとidが多相的に動かないことを確認
$ echo "(fun id -> let x1 = id 1 in let x2 = id (fun x -> 0) in 0) (fun x -> x)" | dune exec tklangc
Fatal error: exception Tklang_compiler.Type_equs.UnifyError
```
