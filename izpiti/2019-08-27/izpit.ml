open List

let odstej_trojici (a1, b1, c1) (a2, b2, c2) = (a1 - a2, b1 - b2, c1 - c2)

let max_rezultat_do_n f n =
  match map f (List.init (succ n) (fun x -> x)) with 
    | x::xs -> fold_left max x xs
    | _ -> failwith "no"

let pocisti_seznam lst = 
  let rec recursor aux = function
    | [] -> List.rev aux
    | (Some x) :: xs -> recursor (x::aux) xs
    | None :: xs -> recursor aux xs in
  recursor [] lst

let preveri_urejenost lst =
  let rec recursor prev_liho prev_sodo = function 
    | [] -> true
    | x::xs when x mod 2 = 0 -> if x > prev_sodo then recursor prev_liho x xs else false
    | x::xs (*when x mod 2 <> 0 *) -> if x < prev_liho then recursor x prev_sodo xs else false
  in recursor max_int min_int

(*Naloga 2*)

type 'a gnezdenje = 
  | Element of 'a
  | Podseznam of 'a gnezdenje list

let gnezdenje_primer = Podseznam [Element 1; Element 2; Podseznam [Element 3; Podseznam [Element 4]; Podseznam []]; Podseznam [Element 5]]

let najvecja_globina gnezdo =
  let rec aux = function
    | Element a -> 0
    | Podseznam [] -> 1
    | Podseznam lst -> 1 + fold_left max 0 (map aux lst) in
  fold_left max 0 (map aux gnezdo)

let preslikaj f gnezdo = 
  let rec aux = function
    | Element a -> Element (f a)
    | Podseznam [] -> Podseznam []
    | Podseznam lst -> Podseznam (map aux lst) in
  map aux gnezdo

let rec splosci = function
  | Element x -> [x]
  | Podseznam xs -> concat @@ map splosci xs

let is_element gnezdo = match gnezdo with Element _ -> true | _ -> false

let alternirajoci_konstruktorji gnezdo = 
  let rec aux was_prev_el = function
    | [] -> true
    | (Element a)::xs when not was_prev_el -> aux true xs
    | (Podseznam lst)::xs when was_prev_el -> aux false xs
    | _ -> false
  in match gnezdo with
    | [] -> true
    | x::xs -> aux (is_element x) xs

let rec zlozi_gnezdenje f acc = function
  | Element x -> f acc x
  | Podseznam l -> fold_left (zlozi_gnezdenje f) acc l

let rec zlozi_preko_gnezdenja f acc gnezdo =
  zlozi_gnezdenje f acc (Podseznam gnezdo)

(*Naloga 3*)
let sample = [[(1, 10); (3, -10)]; [(2, 10); (5, -20)]; [(3, -10)]; [(4, 15)]; [(5, 0)]]

(*Generiramo vse poti, nato izberemo prvo najkrajšo, ki ima pozitivno končno vrednost.*)
(*Predpostavljam, da je končno mesto vedno enako (1 + length sample) - 
  rešitev bi bila poiskati najvišje mesto, v katero lahko potencialno gremo, to vzamemo za začetno mesto,
  ter filtriramo poti, ki imajo element večji ali enak od dolžine arraya. Ideja pa ostane enaka.*)
let mortimer path_list = 
  let path_arr = Array.of_list path_list in (*Da lahko prosto dostopamo do poti iz mest*)
  let len = Array.length path_arr in (*Večkrat nastopi*)
  let paths_from = Array.make (succ len) [] in (*Za memoizacijo - le-ta je tu sicer implicitna, saj sem rekurzijo razvil v zanko*)
  paths_from.(len) <- [[len]]; (*Edina pot iz zadnjega mesta je v le-to, i.e. tu se stvar konča*)
  let i = ref @@ pred len in (*Začnemo na koncu in gremo nazaj*)
  while !i >= 0 do
    let next = path_arr.(!i) |> map fst in
      paths_from.(!i) <- map (fun x -> !i::x) (flatten (map (Array.get paths_from) next));
      (*Vsakič dodamo poti iz prejšnjih mest, lahko naredimo tako, ker so poti vedno naraščajoče.*)
    decr i;
  done;
  let paths = sort (fun x y -> compare (length x) (length y)) paths_from.(0) (*Zanimajo nas poti iz 0, sortiramo jih po dolžini*)
                |> filter (fun x -> mem len x) in (*V primeru slepih ulic*)
  let get_val_between a b = (*Koliko zaslužimo na poti iz a v b; če smo presegli pot privzamemo 0 za lažje računanje*)
    if a >= len then 0 else
      filter (fun (x, _) -> x = b) path_arr.(a) |> hd |> snd in
  let walk path = List.fold_left2 (fun a x y -> a + get_val_between x y) 0 (path) ((tl path) @ [len]) in (*Se sprehodimo po dani poti in seštejemo vrednosti.*)
  let rec finally = function (*Vrnemo prvo pot, ki ima pozitivno vrednost, če je ni, None.*)
    | [] -> None
    | x::xs -> if walk x > 0 then Some x else finally xs in
  finally paths