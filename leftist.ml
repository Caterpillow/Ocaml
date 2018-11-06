(*
  Jolanta Mozyrska
  406254
  Code-reviewer: Małgorzata Biegała
*)

(*pusty wezel reprezentowany przez Null; 
wpp priorytet, lewy, prawy syn, wysokosc*)
type 'a queue = |Null
				|Node of 
					{priority : 'a; 
					left : 'a queue; 
					right : 'a queue; 
					right_height : int;}

exception Empty 

let empty = Null

(*Getters*)
let getHeight node = 
	match node with
	| Null -> 0
	| Node node -> node.right_height

let getLeft node = 
	match node with
	| Null -> raise Empty
	| Node node -> node.left

let getRight node = 
	match node with
	| Null -> raise Empty
	| Node node -> node.right

let getPriority node = 
	match node with
	| Null -> raise Empty
	| Node node -> node.priority

let is_empty this_queue = 
	match this_queue with
	| Null -> true
	| _ -> false

(*tworzy drzewo z nizszym poddrzewem po prawej stronie*) 
let rec change_node my_priority new_left new_right = 
	if (getHeight new_right) > (getHeight new_left)
		then change_node my_priority new_right new_left
	else Node {priority = my_priority; left = new_left; right = new_right; right_height = (getHeight new_right) +1;}

(*łączy drzewa*)
let rec join first_queue second_queue = 
	match (first_queue, second_queue) with
	| (Null, second_queue) -> second_queue
	| (first_queue, Null) -> first_queue
	| (first_queue, second_queue) ->
		(*chcemy mieć wyższy priorytet w
		wierzchołku 1. drzewa*)
		if((getPriority first_queue) > (getPriority second_queue))
			then join second_queue first_queue 
		else 
			(*lewe poddrzewo 1. drzewa*)
			let tree_1 = (getLeft first_queue)
			(*prawe poddrzewo 1. drzewa + 2. drzewo*)
			and tree_2 = join (getRight first_queue) second_queue
			(*bierzemy korzen z 1. drzewa*)
			in change_node (getPriority first_queue) tree_1 tree_2
		
(*dodanie nowego elementu to polaczenie kolejki i kolejki 1-elementowej*)
let add new_priority this_queue = 
	let element = Node { priority = new_priority; left = Null; right = Null; right_height = 1;}
	in join element this_queue

(** Dla niepustej kolejki [q], [delete_min q] zwraca parę [(e,q')] gdzie [e]
    jest elementem minimalnym kolejki [q] a [q'] to [q] bez elementu [e].
    Jeśli [q] jest puste podnosi wyjątek [Empty]. *)
let delete_min this_queue = 
	if is_empty this_queue
		then raise Empty
	else (getPriority (this_queue), join (getLeft this_queue) (getRight this_queue))
(*
open Leftist;;

exception WA;;

(* Returns true if ALL values from q are taken out in the order given in l *)
let test q l =
    try
      let rest_tree = List.fold_left (fun rest_tree head -> 
        let (elem, rest_tree) = delete_min rest_tree
        in 
        if(compare head elem != 0) then raise WA 
        else rest_tree) 
                                    q l
      in
      is_empty rest_tree
    with WA -> false
;;

let q1 = empty |> add 6 |> add 5 |> add 9 
|> add 2 |> add 2 |> add 7 |> add 42
let q2 = empty |> add 1 |> add 2 |> add 3 |> add 4 |> add 5

let q3 = join q1 q2
let q4 = join q3 q3

let l1 = List.sort compare [6; 5; 9; 2; 2; 7; 42]
let l2 = List.sort compare [1; 2; 3; 4; 5]
let l3 = List.sort compare (l1 @ l2)
let l4 = List.sort compare (l3 @ l3);;

assert(is_empty empty);;

assert(test q1 l1);;
assert(test q2 l2);;
assert(test q3 l3);;
assert(test q4 l4);;
assert(not(test q4 l3));;
assert(not(test q3 l4));;
assert(test empty []);;

(*float*)

let q1 = empty |> add 4.3 |> add 1.1 |> add 0.2 |> add 0.01 
|> add 222.1 |> add 42.42 |> add 3.14
let q2 = empty |> add 1.5 |> add 3.3 |> add 3.3 |> add 4.2 |> add 5.1

let q3 = join q1 q2
let q4 = join q3 q3

let l1 = List.sort compare [4.3; 1.1; 0.2; 0.01; 222.1; 42.42; 3.14]
let l2 = List.sort compare [1.5; 3.3; 3.3; 4.2; 5.1]
let l3 = List.sort compare (l1 @ l2)
let l4 = List.sort compare (l3 @ l3);;

assert(test q1 l1);;
assert(test q2 l2);;
assert(test q3 l3);;
assert(test q4 l4);;
assert(not(test q4 l3));;
assert(not(test q3 l4));;

(*string*)

let q1 = empty |> add "aabc" |> add "gg" |> add "acca" 
|> add "baba" |> add "abbabb" |>
add "aabbab" |> add "aaabbaa" |> add "1.23"
let q2 = empty |> add "aab" |> add "aba" |> add "abb" |> add "baa" |> add "bab"

let q3 = join q1 q2
let q4 = join q1 q1
let l1 = List.sort compare ["aabc"; "gg"; "acca"; "baba"; 
"abbabb"; "aabbab"; "aaabbaa"; "1.23"]
let l2 = List.sort compare ["aab"; "aba"; "abb"; "baa"; "bab"]
let l3 = List.sort compare (l1 @ l2)
let l4 = List.sort compare (l1 @ l1);;

assert(test q1 l1);;
assert(test q2 l2);;
assert(test q3 l3);;
assert(test q4 l4);;
assert(not(test q4 l3));;
assert(not(test q3 l4));;
*)