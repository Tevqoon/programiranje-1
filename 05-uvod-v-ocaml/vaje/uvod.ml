
(* ========== Vaja 1: Uvod v OCaml  ========== *)

(*----------------------------------------------------------------------------*]
 Funkcija [square] vrne kvadrat podanega celega števila.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # square 2;;
 - : int = 4
[*----------------------------------------------------------------------------*)

let rec square x = x * x

(*----------------------------------------------------------------------------*]
 Funkcija [middle_of_triple] vrne srednji element trojice.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # middle_of_triple (true, false, true);;
 - : bool = false
[*----------------------------------------------------------------------------*)

let rec middle_of_triple _ b _ = b 

(*----------------------------------------------------------------------------*]
 Funkcija [starting_element] vrne prvi element danega seznama. V primeru
 prekratkega seznama vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # starting_element [1; 2; 3; 4];;
 - : int = 1

[*----------------------------------------------------------------------------*)

let rec starting_element lst = match lst with 
    | [] -> failwith "Podal si prazen seznam"
    | head :: rest -> head

(*----------------------------------------------------------------------------*]
 Funkcija [multiply] zmnoži vse elemente seznama. V primeru praznega seznama
 vrne vrednost, ki je smiselna za rekurzijo.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # multiply [2; 4; 6];;
 - : int = 48
[*----------------------------------------------------------------------------*)

let rec multiply sez = match sez with
    | [] -> 1
    | head :: rest -> head * multiply rest

(*----------------------------------------------------------------------------*]
 Napišite funkcijo ekvivalentno python kodi:

  def sum_int_pairs(pair_list):
      if len(pair_list) == 0:
        return []
      else:
        x, y = pair_list[0]
        return [x + y] + sum_int_pairs(pair_list[1:])

 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # sum_int_pairs [(1, -2); (3, 4); (0, -0)];;
 - : int list = [-1; 7; 0]
[*----------------------------------------------------------------------------*)

let rec sum_int_pairs sez = match sez with
    | [] -> []
    | (a, b) :: rest -> a + b :: sum_int_pairs rest

(*----------------------------------------------------------------------------*]
 Funkcija [get k list] poišče [k]-ti element v seznamu [list]. Številčenje
 elementov seznama (kot ponavadi) pričnemo z 0. Če je k negativen, funkcija
 vrne ničti element. V primeru prekratkega seznama funkcija vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # get 2 [0; 0; 1; 0; 0; 0];;
 - : int = 1
[*----------------------------------------------------------------------------*)

let rec get n lst = match lst with 
  [] -> failwith "Prekratek seznam" 
  | first :: rest -> (if n <= 0 then first else get (n - 1) rest)

let rec get2 n lst = match lst with
  | [] -> failwith "Prekratek seznam"
  | a :: rest when n <= 0 -> a
  | _ :: rest -> get2 (n - 1) rest

(*----------------------------------------------------------------------------*]
 Funkcija [double] podvoji pojavitve elementov v seznamu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # double [1; 2; 3];;
 - : int list = [1; 1; 2; 2; 3; 3]
[*----------------------------------------------------------------------------*)

let rec double = function
  | [] -> []
  | x::xs -> x :: x :: double xs

(*----------------------------------------------------------------------------*]
 Funkcija [insert x k list] na [k]-to mesto seznama [list] vrine element [x].
 Če je [k] izven mej seznama, ga funkcija doda na začetek oziroma na konec.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # insert 1 3 [0; 0; 0; 0; 0];;
 - : int list = [0; 0; 0; 1; 0; 0]
 # insert 1 (-2) [0; 0; 0; 0; 0];;
 - : int list = [1; 0; 0; 0; 0; 0]
[*----------------------------------------------------------------------------*)

let rec insert x k = function
  | [] -> [x] 
  | list when k <= 0 -> x :: list
  | first :: rest -> first :: (insert x (k - 1) rest)

(*----------------------------------------------------------------------------*]
 Funkcija [divide k list] seznam razdeli na dva seznama. Prvi vsebuje prvih [k]
 elementov, drugi pa vse ostale. Funkcija vrne par teh seznamov. V primeru, ko
 je [k] izven mej seznama, je primeren od seznamov prazen.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # divide 2 [1; 2; 3; 4; 5];;
 - : int list * int list = ([1; 2], [3; 4; 5])
 # divide 7 [1; 2; 3; 4; 5];;
 - : int list * int list = ([1; 2; 3; 4; 5], [])
[*----------------------------------------------------------------------------*)

let rec divide k = function
  | [] -> [], []
  | lst when k <= 0 -> [], lst
  | first :: rest -> let prvi, drugi = divide (k - 1) rest in first :: prvi, drugi

(*----------------------------------------------------------------------------*]
 Funkcija [rotate n list] seznam zavrti za [n] mest v levo. Predpostavimo, da
 je [n] v mejah seznama.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # rotate 2 [1; 2; 3; 4; 5];;
 - : int list = [3; 4; 5; 1; 2]
[*----------------------------------------------------------------------------*)

let rec rotate n = function
  | [] -> failwith "n ni v mejah seznama"
  | list when n = 0 -> list
  | first :: rest -> (rotate (n - 1) rest @ [first])

(*----------------------------------------------------------------------------*]
 Funkcija [remove x list] iz seznama izbriše vse pojavitve elementa [x].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # remove 1 [1; 1; 2; 3; 1; 2; 3; 1; 1];;
 - : int list = [2; 3; 2; 3]
[*----------------------------------------------------------------------------*)

let rec remove x list = match list with
  | [] -> []
  | first :: rest when first = x -> remove x rest
  | first :: rest -> first :: remove x rest

(*----------------------------------------------------------------------------*]
 Funkcija [is_palindrome] za dani seznam ugotovi ali predstavlja palindrom.
 Namig: Pomagaj si s pomožno funkcijo, ki obrne vrstni red elementov seznama.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # is_palindrome [1; 2; 3; 2; 1];;
 - : bool = true
 # is_palindrome [0; 0; 1; 0];;
 - : bool = false
[*----------------------------------------------------------------------------*)

let rec is_palindrome list =
  let rec obrni l = match l with
    | [] -> []
    | first :: rest -> (obrni rest) @ [first]
  in list = obrni list

(*----------------------------------------------------------------------------*]
 Funkcija [max_on_components] sprejme dva seznama in vrne nov seznam, katerega
 elementi so večji od istoležnih elementov na danih seznamih. Skupni seznam ima
 dolžino krajšega od danih seznamov.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # max_on_components [5; 4; 3; 2; 1] [0; 1; 2; 3; 4; 5; 6];;
 - : int list = [5; 4; 3; 3; 4]
[*----------------------------------------------------------------------------*)

let rec max_on_components  l1 l2 = match l1, l2 with
  | [], _ -> []
  | _, [] -> []
  | x::xs, y::ys -> (if x > y then x else y) :: max_on_components xs ys

(*----------------------------------------------------------------------------*]
 Funkcija [second_largest] vrne drugo največjo vrednost v seznamu. Pri tem se
 ponovitve elementa štejejo kot ena vrednost. Predpostavimo, da ima seznam vsaj
 dve različni vrednosti.
 Namig: Pomagaj si s pomožno funkcijo, ki poišče največjo vrednost v seznamu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # second_largest [1; 10; 11; 11; 5; 4; 10];;
 - : int = 10
[*----------------------------------------------------------------------------*)

let rec second_largest list =
  let rec r lst current_max = match lst with
    | [] -> current_max
    | x::xs when x >= current_max -> r xs x
    | x::xs -> r xs current_max in
  let max_in_list lst = match lst with
    | [] -> failwith "Cannot find maximum of empty list"
    | x::xs -> r xs x in
  let m2 = max_in_list (remove (max_in_list list) list) in m2 