(*============================================================================*]
 Po koncu karantene načrtuje Rožle planinski pohod po svojem najljubšem
 gorovju. Zamislil si je že pot, ki ima veliko priložnosti za fotografiranje.
 Ker pa uporablja zastarel telefon, ima na pomnilniku prostora za zgolj dve
 fotografiji. Da bi ti dve sliki čim bolj izkoristil, želi da je med lokacijo
 prve fotografije in lokacijo druge fotografije kar se da velik vzpon.

 Kot vhod dobimo seznam nadmorskih višin za vse razgledne točke v takšnem
 vrstnem redu, kot si sledijo po poti. Na primer:

    [350; 230; 370; 920; 620; 80; 520; 780; 630]

 V zgornjem primeru se Rožletu najbolj splača slikati na točki 5 (višina 80m)
 in nato na točki 7 (višina 780m), saj se je med njima vzpel za 700 metrov.
 Čeprav je med točko 3 (višina 920m) in točko 5 (višina 80m) večja višinska
 razlika, se je med točkama spuščal in ne vzpenjal, zato ne prideta v poštev.
[*============================================================================*)

let test = [350; 230; 370; 920; 620; 80; 520; 780; 630]

(*Naivno v O(n^2)*)

let naive lst =
  let arr = Array.of_list lst in
  let l = Array.length arr in
  let memo = Array.make l 0 in
  Array.iteri (fun i x ->
    memo.(i) <- (Array.sub arr (i + 1) (l - i - 1) 
                |> Array.fold_left max 0)
                - x
    ) arr;
  Array.fold_left max 0 memo

(* a *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo, ki v času `O(n log(n))` ali hitreje izračuna največjo
  višinsko razliko med optimalno izbranima točkama. Časovno zahtevnost
  utemeljite v komentarju.
[*----------------------------------------------------------------------------*)

(*Implementirano skozi b), spodaj.*)

(* b *)
(*----------------------------------------------------------------------------*]
  Prejšnjo rešitev prilagodite tako, da vrne zgolj indeksa teh dveh točk. Pri
  tem poskrbite, da ne pokvarite časovne zahtevnosti v `O` notaciji.
[*----------------------------------------------------------------------------*)


(*Seznam sortiramo v O(nlogn), skupaj s prvotnimi indeksi. Nato vzamemo najmanši 
  element in iščemo največji element za vse indekse večje od le-tega. Če obstaja, smo končali.
  Če ne, vzamemo naslednji možni minimum in ponavljamo. Predpostavljam, da obstaja rešitev 
  (ne gremo samo navzdol). Ta zadnji iskalni del je sicer v najslabšem primeru 
  potencialno O(n^2), a v povprečnem primeru, torej, kjer preverimo le nekaj potencialnih minimumov,
  O(n).*)

let indexes lst = 
  let arr = Array.of_list lst in
  let l = pred @@ Array.length arr in
  let with_indeces = arr |> Array.mapi (fun i x -> (i, x)) in
  Array.sort (fun (_, x) (_, y) -> compare x y) with_indeces;
  (*Tu smo sortirali.*)
  let sorted_index = ref (-1) in (*Iteriramo po sortiranih elementih*)
  let min_index = ref 0 in       (*Za vsakega bomo gledali, ali je dober*)
  let max_index = ref 0 in       (*In zanj našli maksimalnega*)
  let continue = ref true in     (*Da vemo, kdaj končamo*)
  (*Vzamemo indekse od minimalnega do konca*)
  while !continue do
    incr sorted_index;
    min_index := fst with_indeces.(!sorted_index); (*Vzamemo naslednji potencialni minimum*)
    for i = !min_index to l do                 (*In zanj iščemo maksimum*)
      if arr.(i) > arr.(!min_index) && arr.(i) > arr.(!max_index) then (*Če ga najdemo, vemo, da je ta minimum dober.*)
        (max_index := i;
         continue := false;)                       (*Z ostalimi iteracijami v for-loopu bomo našli maksimum*)
    done;
  done;
  (!min_index, !max_index)                         (*Naposled vrnemo minimalni in maksimalni indeks*)

(*Zadevo lahko še bolj optimiziramo tako, da za maksimum za dan minimum gledamo sortiran array od
  desne proti levi in vzamemo prvi element, katerega indeks je veljaven. To je moja glavna rešitev.*)

let indexes' lst = 
  let arr = Array.of_list lst in
  let l = pred @@ Array.length arr in
  let with_indeces = arr |> Array.mapi (fun i x -> (i, x)) in
  Array.sort (fun (_, x) (_, y) -> compare x y) with_indeces;
  (*Tu smo sortirali.*)
  let sorted_index = ref (-1) in (*Iteriramo po sortiranih elementih*)
  let min_index = ref 0 in       (*Za vsakega bomo gledali, ali je dober*)
  let max_index = ref 0 in
  let continue = ref true in     (*Da vemo, kdaj končamo*)
  while !continue do
    incr sorted_index;
    min_index := fst with_indeces.(!sorted_index);
    let i = ref l in
    while !i > !min_index && !continue do
      if fst with_indeces.(!i) > !min_index then (max_index := fst with_indeces.(!i); min_index := !min_index; continue := false);
      decr i;
    done;
  done;
  (!min_index, !max_index)

(*Ker najprej sortiramo, in ker dobro postavimo meje, je rešitev garantirana in hitra.*)

let foto lst = 
  let i1, i2 = indexes' lst in
  (List.nth lst i2) - (List.nth lst i1)
