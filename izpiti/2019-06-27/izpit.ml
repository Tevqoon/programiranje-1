open List

type complex = {re :float ; im : float}

let complex_add x y = { re = x.re +. y.re; im = x.im +. y.im}

let complex_conjugamte x = { re = x.re; im = 0. -. x.im }

let list_apply_either pred f g = map (fun x -> if pred x then f x else g x)

let ( *** ) x y = int_of_float (float_of_int x ** float_of_int y)

let eval_poly poly = 
    let rec aux acc index = function
        | [] -> acc
        | x::xs -> aux (acc + x *** index) (succ index) xs
    in aux 0 0 poly

let add_nth n ftuple = match n, ftuple with
    | 0, (x1,x2,x3,x4,x5) -> (succ x1,x2,x3,x4,x5)
    | 1, (x1,x2,x3,x4,x5) -> (x1,succ x2,x3,x4,x5)
    | 2, (x1,x2,x3,x4,x5) -> (x1,x2,succ x3,x4,x5)
    | 3, (x1,x2,x3,x4,x5) -> (x1,x2,x3,succ x4,x5)
    | 4, (x1,x2,x3,x4,x5) -> (x1,x2,x3,x4,succ x5)
    | _ -> failwith "assert not"

let ways tovornost = 
    let zabojniki = [(1,0); (3,1); (4,2); (7,3); (10,4)] in
    let memo = Array.make (succ tovornost) [(0,0,0,0,0)] in
    Array.iteri (fun i _ ->
        memo.(i) <-
            match filter (fun (x,y) -> i >= x) zabojniki 
                    |> map (fun (x,y) -> map (add_nth y) memo.(i - x))
                    |> flatten
            with [] -> memo.(0) | x -> x
                |> fold_left (fun a x -> if mem x a then a else x::a) []
    ) memo; 
    memo.(tovornost)
