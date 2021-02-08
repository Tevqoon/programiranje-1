open List

(* =================== *)
(* 1. naloga: funkcije *)
(* =================== *)

let is_root x y = x * x = y 

let pack3 a b c = (a, b, c) 

let sum_if_not p lst = fold_left (fun a x -> if p x then a else a + x) 0 lst

let apply funcs els = rev @@ rev_map (fun e -> rev @@ rev_map (fun f -> f e) funcs) els

(* ======================================= *)
(* 2. naloga: podatkovni tipi in rekurzija *)
(* ======================================= *)

type vrsta_srecanja = DopolniMe

type srecanje = DopolniMe'

type urnik = DopolniMe''

let vaje = "dopolni me"
let predavanja = "dopolni me"

let urnik_profesor = "dopolni me"

let je_preobremenjen = "dopolni me"

let bogastvo = "dopolni me"
