(*Naloga 1*)
open List (*Lispovske navade ne umrejo.*)

let option_sum a b = match a, b with
    | None, _ | _, None -> None
    | Some x, Some y -> Some (x + y)

let twostep_map f g h a =
    let x, y = f a in
    (g x, h y)

let function_repeat f lst =
    let rec aux acc = function
        | [] -> fold_left (fun x y -> rev_append y x) [] acc 
            (*Praktično rekurzivni concat, ker so elementi v vsakem bloku enaki 
            jih zastonj reversamo, kar nam omogoči repno rekurzijo.*)
        | x::xs -> let n = f x in 
            (*Sama aux funkcija je jasno repno rekurzivna, saj
              v vsakem primeru kličemo le aux funkcijo samo.*)
            if n > 0 then
                aux (List.init n (fun _ -> x) :: acc) xs
            else 
                aux acc xs
    in aux [] lst 

let iterate f p starting = 
    let value = ref starting in
    while not (p !value) do
        value := f !value;
    done; !value

let iteratee f p starting = (*Problem: seq je implementiran rekurzivno, poleg rev in prijateljev *)
    Seq.unfold (fun x -> if p x then None else Some (x, f x)) starting
    |> of_seq |> rev |> hd |> f

let iterateee f p starting = (*Problem: potencialno bi število aplikacij f bilo daljše od max_int *)
    let value = ref starting in
    try
        for i = 0 to max_int do
            value := f !value;
            if p !value then failwith ""; done;
        starting
    with Failure _ -> !value

(*Naloga 2*)

type 'a improved_list = Empty | Node of ('a array * 'a improved_list)

let test = Node ([|1;2;20|], (Node ([|17;19;20;30|], Node ([|100|], Empty))))

let count ilst = 
    let rec aux acc = function
        | Empty -> acc
        | Node(arr, rest) -> aux (acc + Array.length arr) rest
    in aux 0 ilst

let rec nth n = function
    | Empty -> None
    | Node(arr, rest) when n < Array.length arr -> Some arr.(n)
    | Node(arr, rest) -> nth (n - Array.length arr) rest

let is_sorted ilst =
    let rec arr_sorted arr =
        let sorted = ref true in
        for i = 1 to pred (Array.length arr) do
            if arr.(i) < arr.(i - 1) then sorted := false;
        done; !sorted in
    let rec aux prev = function
        | Empty -> true
        | Node(arr, rest) when arr.(0) > prev && arr_sorted arr -> 
            aux (arr.(pred @@ Array.length arr)) rest
        | _ -> false
    in aux min_int ilst

let update ilst index new_val =
    let rec aux n = function
        | Empty -> failwith "Index out of bounds."
        | Node(arr, rest) when n < Array.length arr -> 
            let n_arr = Array.copy arr in n_arr.(n) <- new_val;
            Node (n_arr, rest)
        | Node(arr, rest) -> Node(arr, aux (n - Array.length arr) rest)
    in aux index ilst

(*Naloga 3*)



