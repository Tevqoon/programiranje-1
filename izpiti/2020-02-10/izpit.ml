open List

let dot_prod (x1, y1, z1) (x2, y2, z2) = x1 * x2 + y1 * y2 + z1 * z2

let fix_second f y = (fun x -> f x y)

let combine_and_filter f xs ys =
    let rec aux acc xs ys = match xs, ys with
        | [], _ | _, [] -> rev acc
        | x::xs, y::ys -> match f x y with
            | None   -> aux acc xs ys
            | Some z -> aux (z::acc) xs ys
    in aux [] xs ys 

let conditional_print p slst =
    let filtered = filter p slst in
    List.mapi (fun i x -> if i + 1 < length filtered then x ^ ", " else x) filtered
    |> List.iter (fun s -> print_string s);
    ()

(*-------------------------------------------------------------------------------------------*)

type ('a, 'b) tree =
    | Empty 
    | Anode of ('a, 'b) tree * 'a * ('a, 'b) tree
    | Bnode of ('a, 'b) tree * 'b * ('a, 'b) tree

type result = {aNodes : int; bNodes : int}

let test = Anode (Bnode (Empty, true, Empty), 12, Anode (Anode (Empty, 0, Empty), 5, Bnode (Empty, false, Empty)))
let test' = Anode (Bnode (Empty, true, Empty), 12, Anode (Bnode (Empty, false, Empty), 5, Bnode (Empty, false, Empty)))

let adepth abtree = 
    let rec aux depth = function
        | Empty -> 0
        | Anode (left, _, right) -> max (max depth (aux (succ depth) left)) (aux (succ depth) right)
        | Bnode (left, _, right) -> max (aux (succ depth) left) (aux (succ depth) right)
    in aux 1 abtree

let bdepth abtree = 
    let rec aux depth = function
        | Empty -> 0
        | Bnode (left, _, right) -> max (max depth (aux (succ depth) left)) (aux (succ depth) right)
        | Anode (left, _, right) -> max (aux (succ depth) left) (aux (succ depth) right)
    in aux 1 abtree

let rec is_typemirror tree1 tree2 = match tree1, tree2 with
    | Empty, Empty -> true
    | Anode _, Anode _
    | Bnode _, Bnode _
    | Anode _, Empty
    | Bnode _, Empty
    | Empty, Anode _
    | Empty, Bnode _ -> false
    | Anode (left, _, right), Bnode (left', _, right') 
    | Bnode (left, _, right), Anode (left', _, right') -> is_typemirror left left' && is_typemirror right right'

let rec count = function
    | Empty -> {aNodes = 0; bNodes = 0}
    | Anode (ltree, _, rtree) -> let l, r = count ltree, count rtree in {aNodes = l.aNodes + r.aNodes + 1; bNodes = l.bNodes + r.bNodes}
    | Bnode (ltree, _, rtree) -> let l, r = count ltree, count rtree in {aNodes = l.aNodes + r.aNodes; bNodes = l.bNodes + r.bNodes + 1}

let tree_map fa fb tr =
    let rec aux = function
        | Empty -> Empty
        | Anode (left, a, right) -> Anode (aux left, fa a, aux right)
        | Bnode (left, b, right) -> Bnode (aux left, fb b, aux right)
    in aux tr

let tree_fold fa fb acc tr = 
    let rec aux acc = function
        | Empty -> acc
        | Anode (left, a, right) -> aux (aux (fa acc a) right) left
        | Bnode (left, b, right) -> aux (aux (fb acc b) right) left
    in aux acc tr

let foldmap fa fb acc tr =
    let fa_fold acc a = fst (fa acc a) in
    let fb_fold acc b = fst (fb acc b) in
    let fa_map a = snd (fa acc a) in
    let fb_map b = snd (fb acc b) in
    let final_acc = tree_fold fa_fold fb_fold acc tr in
    let final_tree = tree_map fa_map fb_map tr in
    (final_acc, final_tree)

(*-------------------------------------------------------------------------------------------*)

let f k n = 
    let rec aux = function
        | 1 -> [[0]]
        | n -> List.map (fun x -> List.init (succ k) (fun i -> i + (hd @@ rev x) :: x)) (aux (pred n)) |> flatten |> map rev
    in aux n |> length