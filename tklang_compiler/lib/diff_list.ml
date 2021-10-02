type 'a t = 'a list -> 'a list

let from_list (xs: 'a list): 'a t = List.append xs

let to_list (xs: 'a t): 'a list = xs []

let append (xs: 'a t) (ys: 'a t): 'a t = fun x -> xs (ys x)

let cons (x: 'a) (xs: 'a t): 'a t = append (from_list [x]) xs

let snoc (xs: 'a t) (x: 'a): 'a t = append xs (from_list [x])

let singleton (x: 'a): 'a t = from_list [x]

let empty: 'a t = fun xs -> xs

let concat (xs: ('a t) list): 'a t = List.fold_left append empty xs
