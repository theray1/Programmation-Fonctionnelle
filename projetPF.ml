

(*---types---*)
type transition = {etat_deb:string; entree:string; 
                   sortie:string; etat_fin:string}
;;

type transducteur_fini = Transducteur of 
    (string list * string list *
     string list * string list * 
     string list * transition list)
;; 
type step = {etat:string; mot_entree:string list; mot_sortie:string list}
;;
    
(*---exemple---*)
let alphabet_entree = ["a";"b";"c"]
;; 
let alphabet_sortie = ["A";"B";"C"]
;; 
let etats = ["q0";"q1"]
;;
let etats_init = ["q0"]
;;
let etats_finaux = ["q1"]
;; 
let transition1 = {etat_deb="q0"; entree="a"; sortie="AA"; etat_fin="q1"}
;;
let transition2 = {etat_deb="q1"; entree="b"; sortie="BB"; etat_fin="q0"}
;;
let transitions = [transition1;transition2]
;;
let t1 = Transducteur(alphabet_entree, alphabet_sortie,
                      etats, etats_init, 
                      etats_finaux, transitions)
;;

(*---fonctions utiles---*)
(*OK*)
let print_transition tr = 
  print_string tr.etat_deb;
  print_string " --- ";
  print_string tr.entree;
  print_string ":";
  print_string tr.sortie;
  print_string " ---> ";
  print_string tr.etat_fin;
  print_newline ();
;;
(*OK*)
let print_transitions t = 
  let tr_liste = (match t with 
      | Transducteur(_,_,_,_,_,l) -> l) in
  let rec aux_affiche_transitions t transitions = 
    match transitions with
    | [] -> print_newline ()
    | x::r -> 
        begin
          print_transition x;
          aux_affiche_transitions t r
        end
  in aux_affiche_transitions t tr_liste
;;

print_transitions t1
;;

(* transforme une chaine de caractère en string list *)(*OK*)
(*
  let string_to_list str =
    let taille = String.length str in
    let rec aux str liste pos =
      if(pos=taille) 
      then liste
      else aux str ((String.sub str pos 1)::liste) (pos+1)
    in aux str [] 0
  ;;
  *)
(* transforme une chaine de caractère en string list *)(*OK*)
let string_to_list str =
  let taille = String.length str in
  let rec aux str pos =
    if(pos=taille) 
    then []
    else (String.sub str pos 1)::(aux str (pos+1))
  in aux str 0
;;
string_to_list "abab";;

(* transforme une list de string en string *)(*OK*)
let list_to_string strlist =
  String.concat "" strlist
;;
list_to_string (string_to_list "abab")
;;

(* retourne les états initiaux d'un transducteur *)(*OK*)
let etats_initiaux t = match t with 
  | Transducteur(_,_,_,li,_,_) -> li
;;
let get_transitions t = match t with
  | Transducteur(_,_,_,_,_,tr) -> tr
;;

(*---fonctions pour reconnaitre un mot---*) 

(* créer la ou les premières etapes de la reconnaissance d'un mot *)(*OK*)
let first_steps t mot = 
  let rec aux lei mot = 
    match lei with 
    | [] -> failwith "Il n'y a pas d'états initiaux dans le transducteur"
    | x::[] -> [{etat=x; mot_entree=mot; mot_sortie=[]}]
    | x::r -> {etat=x; mot_entree=mot; mot_sortie=[]}::(aux r mot)
  in aux (etats_initiaux t) mot
;;
let step_init = first_steps t1 (string_to_list "abab")
;;

(* retourne les transitions qui part de l'etat q avec le symbole symb *)(*OK*)
let transition_from_q transitions q symb =
  let rec aux transitions q symb result = 
    match transitions with
    | [] -> result
    | x::r -> aux r q symb (if(x.etat_deb=q && x.entree=symb) then [x] else [])@result
  in aux transitions q symb []
;;
transition_from_q transitions "q0" "a"
;;
transition_from_q transitions "q0" "b"
;;

(* donne le reste du mot a reconnaitre a partir d'une string list *)(*OK*)
let reste_du_mot m = match m with
  | [] -> failwith "Le mot est vide"
  | x::r -> r
;;

(* donne la liste de prochaine étapes de l'évaluation d'un mot a partir d'une étape *)(*OK*)
let next_step t stepbef = 
  let transitions_taken = transition_from_q (get_transitions t) (stepbef.etat) (List.hd stepbef.mot_entree) in
  let rec aux t stepbef transitions result =
    match transitions with
    | [] -> result
    | x::r -> aux t stepbef r ({etat=x.etat_fin; 
                                mot_entree=(reste_du_mot stepbef.mot_entree); 
                                mot_sortie=(x.sortie::stepbef.mot_sortie)}::result)
  in aux t stepbef transitions_taken []
;;
let nextstep1 = next_step t1 (List.hd step_init)
;;

(* donne la liste de prochaine étapes de l'évaluation d'un mot a partir de chaques étapes steps *)(*OK*)
let next_step_plus t steps =
  let rec aux t steps result =
    match steps with
    | [] -> [] 
    | x::r -> if (x.mot_entree=[]) then result
        else (next_step t x)@(aux t r result)
  in aux t steps []
;;
let nextstep1 = next_step_plus t1 step_init
;; 

(* retourne les steps finaux (i.e. les formes normales) *)(*OK*)
let final_steps t mot =
  let step_init = first_steps t (string_to_list mot) in
  
  let rec aux t steps = 
    let ste_next = (next_step_plus t steps) in 
    match ste_next with
    | [] -> steps
    | x::r -> aux t ste_next
  in aux t step_init
;;
final_steps t1 "abab"
;;
final_steps t1 "abbb"
;;

(* retourne la sortie et un booléen *)(**)
let reco t mot =
  let final_st = final_steps t mot in
  
  let rec aux t final_st result= 
    match final_st with
    | [] -> result
    | x::r -> if(x.mot_entree=[]) then (list_to_string x.mot_sortie, true)::(aux t r result)
        else (list_to_string x.mot_sortie, false)::aux t r result
  in aux t final_st []
;;

reco t1 "abab"
;;
reco t1 "abbb"
;;



