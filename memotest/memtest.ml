let memo f =
    let cache = Hashtbl.create 100 in
    let rec mem_f x = 
        match Hashtbl.find_opt cache x with
            | Some y -> y
            | None -> 
                let y = f x in 
                Hashtbl.add cache x y; 
                y
    in mem_f

let podvoji x = 
    print_endline ("Računam: " ^ string_of_int x);
    2 * x

let podvoji = memo podvoji

let rec stevilo_stolpov = function
    | 0 -> 1
    | n when n < 0 -> 0
    | n -> stevilo_stolpov (n - 1)
         + stevilo_stolpov (n - 2) 
         + stevilo_stolpov (n - 3)

let bottom_up_stolpi n = 
    let cache = Array.make 3 1 in
    for i = 3 to n do
        let value = cache.(0) + cache.(1) + cache.(2) in
        cache.(0) <- cache.(1);
        cache.(1) <- cache.(2);
        cache.(2) <- value
    done;
    cache.(2)

let array_stolpi n =
    let cache = Array.make (succ n) 1 in
    for i = 3 to n do
        let value = cache.(i - 1) + cache.(i - 2) + cache.(i - 3) in
        cache.(i) <- value;
    done; cache.(n)

(*Ne shranjuje rezultatov med računanjem*)
let rec mem_stolpi = memo stevilo_stolpov 

let odviti_stolpi f = function
    | 0 -> 1
    | n when n < 0 -> 0
    | n -> f (n - 1) + f (n - 2) + f (n - 3)

let rec pravi_stolpi n = odviti_stolpi pravi_stolpi n