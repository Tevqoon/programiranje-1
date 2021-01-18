(* ========== Vaje 6: Dinamično programiranje  ========== *)


(*----------------------------------------------------------------------------*]
 Požrešna miška se nahaja v zgornjem levem kotu šahovnice. Premikati se sme
 samo za eno polje navzdol ali za eno polje na desno in na koncu mora prispeti
 v desni spodnji kot. Na vsakem polju šahovnice je en sirček. Ti sirčki imajo
 različne (ne-negativne) mase. Miška bi se rada kar se da nažrla, zato jo
 zanima, katero pot naj ubere.

 Funkcija [max_cheese cheese_matrix], ki dobi matriko [cheese_matrix] z masami
 sirčkov in vrne največjo skupno maso, ki jo bo miška požrla, če gre po
 optimalni poti.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # max_cheese test_matrix;;
 - : int = 13
[*----------------------------------------------------------------------------*)

let test_matrix = 
  [| [| 1 ; 2 ; 0 |];
     [| 2 ; 4 ; 5 |];
     [| 7 ; 0 ; 1 |] |]

type path = Empty | Right | Down

let max_cheese matrix = 
  let h = Array.length matrix in
  let memo = Array.make_matrix (h + 1) (h + 1) (0, None) in
  for row_i = (h - 1) downto 0 do
    for col_i = (h - 1) downto 0 do
      let right, path_right = memo.(row_i).(col_i + 1) in
      let down, path_down = memo.(row_i + 1).(col_i) in
      if right > down then
        memo.(row_i).(col_i) <- matrix.(row_i).(col_i) + right, Some Right
      else 
        memo.(row_i).(col_i) <- matrix.(row_i).(col_i) + down, Some Down
    done;
  done;
  let rec walk_path acc x y = match memo.(x).(y) with
    | _, None -> List.rev (List.tl acc)
    | _, Some Down -> walk_path (Down::acc) (x + 1) y
    | _, Some Right -> walk_path (Right::acc) x (y + 1)
    | _ -> failwith "how'd'ya get here"
  in walk_path [] 0 0

(*Najdaljše naraščajoče podzaporedje*)

let test_list = [|2;3;6;8;4;4;6;7;12;8;9|]
(* rezultat [2, 3, 4, 4, 6, 7, 8, 9]. *)
let test_list' = [|1;2;3;4;5;0|]

let descending_subseq_from array start = 
  let ret = Stack.create () in
  Stack.push array.(start) ret;
  for i = (pred start) downto 0 do
    if array.(i) <= Stack.top ret then Stack.push array.(i) ret;
  done;
  ret |> Stack.to_seq |> List.of_seq

let subseq arr = 
  let l = Array.length arr in
  Array.init l (fun i -> descending_subseq_from arr i)
  |> Array.fold_left (fun x y -> if List.length y > List.length x then y else x) []

(*----------------------------------------------------------------------------*]
 Rešujemo problem sestavljanja alternirajoče obarvanih stolpov. Imamo štiri
 različne tipe gradnikov, dva modra in dva rdeča. Modri gradniki so višin 2 in
 3, rdeči pa višin 1 in 2.

 Funkcija [alternating_towers] za podano višino vrne število različnih stolpov
 dane višine, ki jih lahko zgradimo z našimi gradniki, kjer se barva gradnikov
 v stolpu izmenjuje (rdeč na modrem, moder na rdečem itd.). Začnemo z gradnikom
 poljubne barve.

 Namig: Uporabi medsebojno rekurzivni pomožni funkciji z ukazom [and].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # alternating_towers 10;;
 - : int = 35
[*----------------------------------------------------------------------------*)


(*----------------------------------------------------------------------------*]
 Na nagradni igri ste zadeli kupon, ki vam omogoča, da v Mercatorju kupite
 poljubne izdelke, katerih skupna masa ne presega [max_w] kilogramov. Napišite
 funkcijo [best_value articles max_w], ki poišče največjo skupno ceno, ki jo
 lahko odnesemo iz trgovine, kjer lahko vsak izdelek vzamemo večkrat, nato pa
 še funkcijo [best_value_uniques articles max_w], kjer lahko vsak izdelek
 vzamemo kvečjemu enkrat.

 Namig: Modul [Array] ponuja funkcije kot so [map], [fold_left], [copy] in
 podobno, kot alternativa uporabi zank.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # best_value articles 1.;;
 - : float = 10.95
 # best_value_unique articles 1.;;
- : float = 7.66
[*----------------------------------------------------------------------------*)

(* Articles are of form (name, price, weight) *)
let articles = [|
	("yoghurt", 0.39, 0.18);
	("milk", 0.89, 1.03);
  ("coffee", 2.19, 0.2);
  ("butter", 1.49, 0.25);
  ("yeast", 0.22, 0.042);
  ("eggs", 2.39, 0.69);
  ("sausage", 3.76, 0.50);
  ("bread", 2.99, 1.0);
  ("Nutella", 4.99, 0.75);
  ("juice", 1.15, 2.0)
|]


(*----------------------------------------------------------------------------*]
 Cena sprehoda po drevesu je vsota vrednosti v vseh obiskanih vozliščih.
 Poiščite vrednost najdražjega sprehoda od korena do listov drevesa.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # max_path Empty ;;
 - : 'a option = None
 # max_path test_tree;;
- : int option = Some 21
[*----------------------------------------------------------------------------*)

type 'a tree
 = Empty
 | Node of ('a tree) * 'a * ('a tree)

let leaf x = Node (Empty, x, Empty)

let test_tree = Node( Node(leaf 0, 2, leaf 13), 5, Node(leaf 9, 7, leaf 4))

(*----------------------------------------------------------------------------*]
 Cena sprehoda po drevesu je vsota vrednosti v vseh obiskanih vozliščih.
 Poiščite najdražji sprehod od korena do listov drevesa: Funkcija pot vrne v 
 obliki seznama smeri, katere je potrebno izbrati za najdražji sprehod.

 Napišite tudi funkcijo, ki sprehod pretvori v elemente sprehoda
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # max_path_trace Empty ;;
 - : 'a list = []
 # max_path_trace test_tree;;
- : direction list = [Right, Left]
 # reconstruct test_tree (max_path_trace test_tree);;
- : int list = [5; 7; 9]
[*----------------------------------------------------------------------------*)

type direcion 
  = Left
  | Right