open List

(* a *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo, ki vrne razliko med produktom in vsoto dveh celih števil.

    razlika_produkta_in_vsote : int -> int -> int

[*----------------------------------------------------------------------------*)

let razlika_produkta_in_vsote x y = (x * y) - (x + y)

(* b *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo, ki združi dva para v četverico.
    
    zlimaj_para : 'a * 'b -> 'c * 'd -> 'a * 'b * 'c * 'd

[*----------------------------------------------------------------------------*)

let zlimaj_para (x1, y1) (x2, y2) = (x1, y1, x2, y2)

(* c *)
(*----------------------------------------------------------------------------*]
  Imamo podatke tipa [int option * int option * int option], ki jih želimo
  grafično predstaviti. Napišite funkcijo [trojica_graficno], ki sprejme takšno
  trojico in vrne niz, kjer so ``manjkajoči'' elementi nadomeščeni z [-].
  Primer vrnjenega niza je ["(1, 2, -)"]

    trojica_graficno : int option * int option * int option -> string

[*----------------------------------------------------------------------------*)

let trojica_graficno (x, y, z) = 
  let lst = [x; y; z] in
  let thingies = map (function None -> "-" | Some x -> string_of_int x) lst
     |> String.concat ", " in
    "(" ^ thingies ^ ")"


(* d *)
(*----------------------------------------------------------------------------*]
  Klic funkcije [nedeljivo_do x n] preveri, da število [x] ni deljivo z nobenim
  naravnim številom od 2 do vključno [n]. Število 73859 je praštevilo, torej
  mora [nedeljivo_do 73859 73858] vrniti [true].

    nedeljivo_do : int -> int -> bool

[*----------------------------------------------------------------------------*)

let nedeljivo_do x n =
  let nedeljivo = ref true in
  for i = 2 to n do
    if x mod i = 0 then nedeljivo := false
  done;
  !nedeljivo

(* e *)
(*----------------------------------------------------------------------------*]
  Seznam elementov tipa ['a option] želimo razdeliti na podsezname glede na
  pojavitve vrednosti [None].

    razcepi_pri_None : 'a option list -> 'a list list.

  Kot primer, funkcija seznam

    [Some 1; None; Some 2; Some 3; None; None; Some 4; None]

  razcepi v [[1]; [2;3]; []; [4]; []]. Funkcija naj bo repno rekurzivna.
[*----------------------------------------------------------------------------*)

let razcepi_pri_None lst =
  let rec aux acc1 acc2 = function
    | [] -> rev ((rev acc2)::acc1)
    | None::xs -> aux ((rev acc2)::acc1) [] xs
    | (Some x)::xs -> aux acc1 (x::acc2) xs
  in aux [] [] lst
