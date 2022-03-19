type alphabet = string list;;

(*type state = Initial of string | Final of string | Normal of string;;*)

type statesSet = string list;;

type transition = Transition of string * string * string * string;; 
(* Une transition est un quadruplet composé 
des éléments suivants :
- état de départ
- étiquette d'entrée
- étiquette de sortie
- état de fin*)

type relation = transition list;;

type fst = Transducteur of (alphabet * alphabet * statesSet * statesSet * statesSet * relation);;
(* Un transducteur à états finis est un sextuplet composé des éléments suivants, ordonnés :
- alphabet d'entrée
- alphabet de sortie
- ensemble des états
- ensemble des états initiaux
- ensemble des états finaux
- ensemble des transitions
*)

let print_bool b =
  if b then print_string "true" else print_string "false";
  print_newline ();;

let get_alphabet_entree t =
  match t with (x,_,_,_,_,_) -> x;;

let get_alphabet_sortie t=
  match t with (_,x,_,_,_,_) -> x;;

let get_q t =
  match t with (_,_,x,_,_,_) -> x;;

let get_i t =
  match t with (_,_,_,x,_,_) -> x;;

let get_f t =
  match t with (_,_,_,_,x,_) -> x;;

let get_relation t =
  match t with (_,_,_,_,_,x) -> x;;

let rec test_egalite_transition r1 r2 =
  match r1 with Transition(a,b,c,d) -> 
    begin
      match r2 with Transition(e,f,g,h) -> ((a=e) && (b=f) && (c=g) && (d=h))
    end
  ;;

let rec contient_transition relation transition =
  match relation with
  |[] -> false
  |t::[] -> test_egalite_transition t transition
  |t::tl -> if (test_egalite_transition t transition) then true else (contient_transition tl transition)
  ;;




(*Instance*)
let a1 = ("a"::"b"::"c"::[]);;
let a2 = ("d"::"e"::"f"::[]);;

let q1 = "I";;
let q2 = "F";;
let q3 = "Q";;

let q = (q1::q2::q3::[]);;
let i = [q1];;
let f = [q2];;

let t1 = Transition(q1, "a", "d", q3);;
let t2 = Transition(q3, "b", "e", q1);;

let relation = (t1::t2::[]);;

let fst1 = Transducteur(a1, a2, q, i ,f, relation);;

print_bool (contient_transition relation t1)