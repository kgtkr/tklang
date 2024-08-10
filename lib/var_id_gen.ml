module M = Map.Make(String)

type t = {
  counter: int;
  dict: Var_id.t M.t;
  dict_acc: Var_id.t M.t;
}

let make: t = { counter = 0; dict = M.empty; dict_acc = M.empty; };;
let gen (ident: string) (gen: t): (Var_id.t * t) =
  let var_id = Var_id.VarId gen.counter in
  (var_id, { counter = gen.counter + 1; dict = M.add ident var_id gen.dict; dict_acc = M.add ident var_id gen.dict_acc; })

let lookup_exn (ident: string) (state: t): Var_id.t = M.find ident state.dict
let scoped (v: ('a * t)) (gen: t): ('a * t) =
  let dict = gen.dict in
  let (x, gen) = v in
  (x, { gen with dict = dict })
