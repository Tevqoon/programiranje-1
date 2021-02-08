open List

let podvoji_vsoto x y = 2 * (x + y)

let povsod_vecji (x1, y1, z1) (x2, y2, z2) = x1 > x2 && y1 > y2 && z1 > z2

let uporabi_ce_lahko f = function
    | None -> None
    | Some x -> Some (f x)

let pojavi_dvakrat a lst = 
    let n = ref 0 in
    iter (fun x -> if x = a then incr n) lst;
    !n = 2

let izracunaj_v_tocki x funcs = rev @@ rev_map (fun f -> f x) funcs

let eksponent x p = int_of_float (float_of_int x ** float_of_int p)

let eksponent' x p = 
    let rec aux acc = function
        | 0 -> 1
        | 1 -> acc
        | n when n mod 2 = 0 -> aux (acc * acc) (n / 2)
        | n                  -> aux (acc * acc * acc) (n / 2)
    in aux x p

type 'a mm_drevo = Empty | Node of 'a mm_drevo * 'a * int * 'a mm_drevo

let rec vstavi drevo el = match drevo with
    | Empty -> Node (Empty, el, 1, Empty)
    | Node (left, x, n, right) when x = el -> Node (left, x, succ n, right)
    | Node (left, x, n, right) when el < x -> Node (vstavi left el, x, n, right)
    | Node (left, x, n, right) -> Node (left, x, n, vstavi right el)

let multimnozica_iz_seznama lst = 
    let tree = ref Empty in
    iter (fun x -> tree := vstavi !tree x) lst;
    !tree

let test = multimnozica_iz_seznama [2;5;1;4;1;1;2;8;8]
let test' = multimnozica_iz_seznama [2;1;4;6;1;1;2;4;5;1;1;2;6;7;9;2;1;3;6;4;2;4;6]

let rec velikost_multimnozice = function
        | Empty -> 0
        | Node (left, _, n, right) -> velikost_multimnozice left + n + velikost_multimnozice right

let rec seznam_iz_multimnozice = function
    | Empty -> []
    | Node (left, x, n, right) -> seznam_iz_multimnozice left 
                                @ init n (Fun.const x) 
                                @ seznam_iz_multimnozice right

let seznam_iz_multimnozice' tree =
    let rec walker acc todo tree = match tree, todo with
        | Empty, [] -> acc
        | Empty, x::xs -> walker acc xs x
        | Node (left, x, n, right), todo -> walker ((init n (Fun.const x))::acc) (left::todo) right
    in
    let rec builder acc = function
        | [] -> rev acc
        | x::xs -> builder (rev_append x acc) (rev xs)
    in 
    builder [] (walker [] [] tree)
