open List

(*============================================================================*]
  Filip potrebuje pomoč pri organiziranju kuhinje. Posode in omare mu je uspelo
  popisati in ustrezno označiti, sedaj pa mora nad tem izvajati kopico
  arhivskih nalog, kjer nastopite vi.
[*============================================================================*)

type 'a vsebina_kuhinje =
  | Ponev of 'a
  | Lonec of 'a * 'a
  | Omara of 'a list

(* a *)
(*----------------------------------------------------------------------------*]
  Definirajte primer seznama kuhinjskih elementov [kuhinja], kjer ponev vsebuje
  niz "tuna", lonec vsebuje "brokoli" in "mango", omara pa vsebuje "sir",
  "toast", "sok" in "ragu".
[*----------------------------------------------------------------------------*)

let kuhinja = [Ponev "tuna"; Lonec ("brokoli", "mango"); Omara ["sir"; "toast"; "sok"; "ragu"]]


(* b *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo [prestej], ki za podani seznam kuhinjskih elementov vrne
  skupno število vsebinskih elementov. Za zgornji primer [kuhinja] bi tako
  vrnila 7.
[*----------------------------------------------------------------------------*)

let prestej kuhinja = 
  map (function 
        | Ponev _ -> 1
        | Lonec _ -> 2
        | Omara lst -> length lst) kuhinja
  |> fold_left (+) 0


(* c *)
(*----------------------------------------------------------------------------*]
  Definirajte funkcijo, ki sprejme funkcijo [f] in kuhinjski element ter
  funkcijo [f] uporabi na celotni vsebini elementa.

    pretvori : (’a -> ’b) -> ’a kuhinjski_element -> ‘b kuhinjski_element

[*----------------------------------------------------------------------------*)

let pretvori f = function
  | Ponev x -> Ponev (f x)
  | Lonec (x, y) -> Lonec (f x, f y)
  | Omara lst -> Omara (map f lst)

(* d *)
(*----------------------------------------------------------------------------*]
  Definirajte funkcijo [pospravi], ki sprejme seznam kuhinjskih elementov in
  vsebino vseh elementov pospravi v eno samo [Omaro]. Vrstni red elementov v
  končni omari je nepomemben. Za vse točke naj bo funkcija repno rekurzivna. 

    pospravi : ’a kuhinjski_element list -> ‘a kuhinjski_element

[*----------------------------------------------------------------------------*)

let pospravi kuhinja =
  let rec aux elacc lstacc = function
    | [] -> rev_append (fold_left rev_append [] lstacc) elacc
    | (Ponev x)::xs -> aux (x::elacc) lstacc xs
    | (Lonec (x, y))::xs -> aux (x::y::elacc) lstacc xs
    | (Omara lst)::xs -> aux elacc (lst::lstacc) xs
  in Omara (aux [] [] kuhinja)
(*Neprazne opcije so repno rekurzivne, rev_append in fold_left sta tudi*)

(* e *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo [oceni], ki sprejme seznam tipa ['a kuhinjski_element list]
  in cenilko vsebine tipa [‘a -> int]. Funkcija izračuna skupno ceno celotnega
  seznama, kjer je cena vsebine v loncih množena s 3, v omarah pa s 5.

  Ocena testne kuhinje za cenilko [String.length] je 115.
[*----------------------------------------------------------------------------*)

let oceni kuhinja f = 
  map (function
    | Ponev x -> f x
    | Lonec (x, y) -> 3 * (f x + f y)
    | Omara lst -> map (fun x -> 5 * (f x)) lst |> fold_left (+) 0
  ) kuhinja 
  |> fold_left (+) 0