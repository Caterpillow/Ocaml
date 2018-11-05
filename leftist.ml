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
	let element = Node { priority = new_priority; left = Null; right = Null; right_height = 0;}
	in join element this_queue

(** Dla niepustej kolejki [q], [delete_min q] zwraca parę [(e,q')] gdzie [e]
    jest elementem minimalnym kolejki [q] a [q'] to [q] bez elementu [e].
    Jeśli [q] jest puste podnosi wyjątek [Empty]. *)
let delete_min this_queue = 
	if is_empty this_queue
		then raise Empty
	else (getPriority (this_queue), join (getLeft this_queue) (getRight this_queue))