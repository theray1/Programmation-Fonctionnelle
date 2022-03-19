type alphabet = Alphabet of (string list);;

type state = Initial of string | Final of string | Normal of string;;

type statesSet = state list;;

type transition = Transition of state * string * string * state;; 
(* Une transition est un quadruplet composé 
des éléments suivants :
- état de départ
- étiquette d'entrée
- étiquette de sortie
- état de fin*)

type relation = Relation of transition list;;

type fst = Transducteur of (alphabet * alphabet * statesSet * statesSet * statesSet * relation);;
(* Un transducteur à états finis est un sextuplet composé des éléments suivants, ordonnés :
- alphabet d'entrée
- alphabet de sortie
- ensemble des états
- ensemble des états initiaux
- ensemble des états finaux
- ensemble des transitions
*)

let a1 = Alphabet(("a"::"b"::"c"::[]));;
let a2 = Alphabet(("d"::"e"::"f"::[]));;

let q1 = Initial("I");;
let q2 = Final("F");;
let q3 = Normal("Q");;

let q = (q1::q2::q3::[]);;
let i = [q1];;
let f = [q2];;

let t1 = Transition(q1, "a", "d", q3);;
let t2 = Transition(q3, "b", "e", q1);;

let relation = Relation((t1::t2::[]));;

let fst1 = Transducteur(a1, a2, q, i ,f, relation);;