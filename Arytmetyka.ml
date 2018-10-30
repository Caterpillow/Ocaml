(*Jolanta Mozyrska
	406254
Code reviewer: Lara Citko*)
open List
type wartosc  = |Przedzial of float * float
				|Dopelnienie of float * float

(*Konstruktory*)
let wartosc_dokladnosc x p = 
	let odchylenie = abs_float(p *. 0.01 *. x)
	in Przedzial(x-.odchylenie, x+.odchylenie)

let wartosc_od_do x y = 
	Przedzial(x,y)

let wartosc_dokladna x = 
	Przedzial (x, x)

(*Selektory*)
let min_wartosc x = 
	match x with
		| Przedzial(a,b) -> a
		| Dopelnienie(_) -> neg_infinity

let max_wartosc x = 
	match x with 
		| Przedzial(a,b) -> b
		| Dopelnienie(_) -> infinity

let sr_wartosc x = 
	match x with
	| Dopelnienie(_) -> nan
	| Przedzial(a,b) -> 
		if a=neg_infinity && b=infinity
			then nan
		else
			(a+.b)/.2.

let in_wartosc x liczba_zawierana = 
	match x with
	|Dopelnienie(a,b) -> 
		liczba_zawierana <= a || liczba_zawierana >= b
	|Przedzial(a,b) ->
		liczba_zawierana >= a && liczba_zawierana <= b

(*Modyfikatory*)
let zamien a b funkcja = 
	match b with
	| Przedzial(_) -> funkcja b a
	| _ -> funkcja a b

(* poprawia Dopełnienie, jeśli przedziały nachodzą na siebie, np. (-inf, 4) suma (3, inf)*)
let popraw mniejsze_od wieksze_od= 
	if mniejsze_od >= wieksze_od
		then Przedzial(neg_infinity,infinity)
	else Dopelnienie(mniejsze_od, wieksze_od)

(*jeśli któraś z liczb przy operacji to Przedzial(nan,nan), otrzymamy true*)
let czy_nan a b =
	match (a,b) with
		|(Przedzial(a_pocz, a_kon),Przedzial(b_pocz,b_kon)) ->
			if not(a_pocz = a_pocz) || not(b_pocz = b_pocz)
				then true
			else false
		|(Przedzial(a_pocz, a_kon),_) ->
			if not(a_pocz = a_pocz) 
				then true
			else false
		|(_,Przedzial(b_pocz,b_kon)) ->
			if not(b_pocz = b_pocz)
				then true
			else false 
		|(_,_) -> false

let rec plus a b = 
	if czy_nan a b then Przedzial(nan,nan)
	else
		match a with
		| Przedzial(a_pocz, a_kon) ->
			(match b with 
				(*dodaj min+min, max+max*)
				|Przedzial(b_pocz,b_kon) -> Przedzial(a_pocz+.b_pocz, a_kon+.b_kon)
				(*jak najbardziej rozpycha się przedział (-inf, mniejsze_od) i (wieksze_od, inf)*)
				|Dopelnienie(mniejsze_od, wieksze_od) -> 
					popraw(mniejsze_od+.a_kon) (wieksze_od+.a_pocz))
		(* a to Dopelnienie*)		
		| _ -> 
			match b with
				|Przedzial(_) -> plus b a
				(*oba to dopełnienia, otrzymujemy każdą liczbę*)
				|_ -> Przedzial(neg_infinity,infinity)

let przeciwna b = 
	match b with
		|Przedzial(b_pocz, b_kon) -> Przedzial(-.b_kon,-.b_pocz)
		|Dopelnienie(mniejsze_od, wieksze_od) -> Dopelnienie(-.wieksze_od,-.mniejsze_od)

let minus a b = plus a (przeciwna b)
	
let rec razy a b = 
	if czy_nan a b then Przedzial(nan,nan)
	else
		match (a,b) with
			(*wybieramy skrajne przedziały, jesli a =(neg_inf, inf), b=(0,0), wynik = (nan,nan)*)
			|(Przedzial(a_pocz, a_kon), Przedzial(b_pocz,b_kon)) -> 
				if (a_pocz = a_kon && a_kon=0.) || (b_pocz = b_kon && b_kon=0.)
					then Przedzial(0.,0.)
				else
					(*wyrzucam nan = 0* infinity, bo nan !=nan*)
					let lista = filter (fun x -> x=x)
						[(a_pocz*.b_kon);(a_kon*.b_pocz);(a_kon*.b_kon);(a_pocz*.b_pocz)]
					in let minimum = fold_left  min infinity lista
					in let maximum = fold_left  max neg_infinity lista 
					in Przedzial(minimum,maximum)
			
			|(Przedzial(a_pocz, a_kon), Dopelnienie(b_mniejsze_od,b_wieksze_od)) ->
				if a_pocz = 0. && a_kon = 0. then Przedzial(0.,0.) (*mnozenie razy 0*)
			(*mnozymy eps(z jednej lub drugiej strony 0) i otrzymujemy kazda liczbe rzeczywista*)
				else if in_wartosc a 0. then Przedzial(neg_infinity,infinity) 
				else if a_pocz > 0.
					then
						let maximum = max(a_pocz*.b_mniejsze_od) (a_kon*.b_mniejsze_od)
						and minimum = min(a_kon*.b_wieksze_od) (a_pocz*.b_wieksze_od)
						in Dopelnienie(maximum, minimum)
				else(* przedział a cały ujemny, np. (-2,-1) i (-inf, 2),(3,inf)*)
					let maximum = max(a_pocz*.b_wieksze_od) (a_kon*.b_wieksze_od)
					in let minimum = min(a_kon*.b_mniejsze_od) (a_pocz*.b_mniejsze_od)
					in Dopelnienie(maximum, minimum)

			|(Dopelnienie(_),Przedzial(_)) -> razy b a
  
			|(Dopelnienie(a_mniejsze_od,a_wieksze_od), Dopelnienie(b_mniejsze_od,b_wieksze_od)) ->
				if in_wartosc a 0. then Przedzial(neg_infinity,infinity)
			else (*jeśli dopełnienia nie zawieraja zera, bierzemy najszersze przedziały*)
				let maximum = max(a_wieksze_od*.b_mniejsze_od) (a_mniejsze_od*.b_wieksze_od)
				in let minimum = min(a_mniejsze_od*.b_mniejsze_od) (a_wieksze_od*.b_wieksze_od)
				in Dopelnienie(maximum, minimum)

let odwroc a = 
	match a with
	|Dopelnienie(mniejsze_od,wieksze_od) -> 
		if mniejsze_od*.wieksze_od < 0. 
			then Przedzial(1./.mniejsze_od, 1./.wieksze_od)
		else 
			Dopelnienie(min(1./.mniejsze_od) (1./.wieksze_od), 
						max(1./.mniejsze_od) (1./.wieksze_od))

	|Przedzial(pocz, kon) ->
		(*przedział zawiera 0, więc bedzie zawierac obie nieskonczonosci *)
		if pocz*.kon<0.  
			then Dopelnienie(1./.pocz,1./.kon)(**)
		(*dzielenie przez zero, więc po domnożeniu razy nan otrzymam nan*)
		else if pocz=kon && kon=0. 
			then Przedzial(nan,nan)
		else if pocz = 0.
			then Przedzial(1./.kon, infinity)
		else if kon = 0.
			then Przedzial(neg_infinity, 1./.pocz)
		(*otrzymamy przedzial od neg_inf do liczby lub od liczby do inf lub od liczby do liczby*)
		else  
			Przedzial(min(1./.pocz)(1./.kon), max(1./.pocz)(1./.kon))

let podzielic a b = 
	match b with
	| Przedzial(0.,0.) -> Przedzial(nan,nan)
	|_ -> razy a (odwroc b)

(* Testy:
	let ( =. ) (x : float) (y : float) =
let e = 1e-6 and d = x -. y in ~-.e < d && d < e;;
let a = max_wartosc ( minus ( wartosc_dokladnosc (2.000000) 
	(1.000000) ) ( minus ( wartosc_dokladnosc (0.000000) (0.000000) )
	 ( podzielic ( wartosc_dokladna (3.000000) ) 
	 	( wartosc_dokladnosc (-7.000000) (2.000000) ) ) ) ) ;;
assert (a =. 1.5998319327731092);;
let a = max_wartosc ( minus ( wartosc_dokladnosc (-5.000000) 
	(3.000000) ) ( wartosc_od_do (-6.000000) (-1.000000) ) ) ;;
assert (a =. 1.15000000000000036);;
let a = in_wartosc ( podzielic ( podzielic ( wartosc_dokladnosc 
	(4.000000) (3.000000) ) ( wartosc_dokladna (-3.000000) ) ) 
	( podzielic ( wartosc_dokladna (-7.000000) ) ( wartosc_dokladna (4.000000) ) ) )
	 (-4.000000);;
assert (a = false);;
let a = min_wartosc ( podzielic ( wartosc_dokladna (0.000000) ) 
	( wartosc_dokladna (4.000000) ) ) ;;
assert (a =. 0.);;
let a = in_wartosc ( razy ( wartosc_dokladna (-2.000000) ) 
	( wartosc_od_do (-1.000000) (2.000000) ) ) (2.000000);;
assert (a = true);;
let a = max_wartosc ( podzielic ( wartosc_dokladnosc (0.000000) (0.000000) )
 ( wartosc_dokladna (4.000000) ) ) ;;
assert (a =. 0.);;
let a = in_wartosc ( podzielic ( wartosc_dokladnosc (1.000000) (0.000000) ) 
	( wartosc_od_do (5.000000) (8.000000) ) ) (-8.000000);;
assert (a = false);;
let a = in_wartosc ( razy ( wartosc_dokladnosc (1.000000) (0.000000) ) 
	( wartosc_dokladna (2.000000) ) ) (2.000000);;
assert (a = true);;
let a = sr_wartosc ( razy ( wartosc_dokladna (6.000000) ) 
	( wartosc_dokladna (0.000000) ) ) ;;
assert (a =. 0.);;
let a = in_wartosc ( podzielic ( wartosc_dokladnosc (-10.000000) (0.000000) ) 
( minus ( wartosc_dokladnosc (0.000000) (4.000000) ) ( wartosc_od_do (7.000000) 
(7.000000) ) ) ) (0.000000);;
assert (a = false);;
let a = sr_wartosc ( podzielic ( wartosc_dokladna (8.000000) ) 
	( wartosc_dokladna (0.000000) ) ) ;;
assert ((classify_float a) == FP_nan);;*)
