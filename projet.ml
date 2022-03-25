(*type alphabet = string list;;*)

(*type state = Initial of string | Final of string | Normal of string;;*)

(*type statesSet = string list;;*)

type transition = Transition of string * string * string * string;; 
(* Une transition est un quadruplet composé 
des éléments suivants :
- état de départ
- étiquette d'entrée
- étiquette de sortie
- état de fin*)

type relation = transition list;;

type fst = Transducteur of (string list * string list * string list * string list * string list * transition list);;
(* Un transducteur à états finis est un sextuplet composé des éléments suivants, ordonnés :
- alphabet d'entrée
- alphabet de sortie
- ensemble des états
- ensemble des états initiaux
- ensemble des états finaux
- ensemble des transitions
*)

let afficher_transition transition =
  match transition with
  |Transition(dep, ent, sor, fin) -> 
    begin
      print_string "(";
      print_string dep;
      print_string ", ";
      print_string ent;
      print_string ", ";
      print_string sor;
      print_string ", ";
      print_string fin;
      print_string ")";
      print_newline ();

    end
  ;;

let afficher_fst transducteur =
  match transducteur with
  |Transducteur(a1,a2,q,i,f,r) ->
    begin
      print_string "alphabet d'entrée :";
      print_newline ();
      List.map print_string a1;
      print_newline ();

      print_string "alphabet de sortie :";
      print_newline ();
      List.map print_string a2;
      print_newline ();

      print_string "états :";
      print_newline ();
      List.map print_string q;
      print_newline ();
      
      print_string "états initiaux :";
      print_newline ();
      List.map print_string i;
      print_newline ();
      
      print_string "états finaux :";
      print_newline ();
      List.map print_string f;
      print_newline ();
      
      print_string "transitions :";
      print_newline ();
      List.map afficher_transition r;
    end 
  ;;

let print_bool b =
  if b then print_string "true" else print_string "false";
  print_newline ();;

let get_alphabet_entree t =
  match t with Transducteur(x,_,_,_,_,_) -> x;;

let get_alphabet_sortie t=
  match t with Transducteur(_,x,_,_,_,_) -> x;;

let get_q t =
  match t with Transducteur(_,_,x,_,_,_) -> x;;

let get_i t =
  match t with Transducteur(_,_,_,x,_,_) -> x;;

let get_f t =
  match t with Transducteur(_,_,_,_,x,_) -> x;;

let get_relation t =
  match t with Transducteur(_,_,_,_,_,x) -> x;;

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
  |t::tl -> if (test_egalite_transition t transition) 
            then true 
            else (contient_transition tl transition)
  ;;

let rec supprime_doublons relation =

  print_string "DEBUT APPEL SUPPRIME DOUBLONS";
  print_newline ();
  List.map afficher_transition relation;
  print_string "we got there";
  print_newline ();
  match relation with
  |[] -> begin print_string "FIN DE LA LOOP NORMALEMENT";print_newline ();[] end
  |t::r -> begin
          print_string "we got further";
          print_newline ();
          if contient_transition r t
          then supprime_doublons r 
          else begin t::(supprime_doublons r) end
        end
        ;;

(*ajout_transitivite renvoie la liste des transitions manquantes pour que obtenir la cloture transitive de la relation de transducteur*)
let ajout_transitivite transducteur = 
  let rel = (get_relation transducteur) in

  (*retourne toutes les transitions dont l'état de départ est l'état de fin de #transition, dans #relation*)
  let rec ajout_transitivite_depuis_une_transition transition relation =
    match relation with
    |[] -> []
    |Transition(dep1, ent1, sor1, fin1)::r -> 
    begin
      match transition with Transition(dep2, ent2, sor2, fin2) -> 
        begin
          if fin2 = dep1 
          then (Transition(dep2, ent2^ent1, sor2^sor1, fin1))::(ajout_transitivite_depuis_une_transition transition r) 
          else (ajout_transitivite_depuis_une_transition transition r)
        end
    end
  in

  (*appelle ajout_transitive_depuis_une_transition sur toutes les transitions de #relation*)
  let rec iteration_cloture relation_iteree relation_complete =
    match relation_iteree with
    |[] -> []
    |t::r -> (ajout_transitivite_depuis_une_transition t relation_complete)@(iteration_cloture r relation_complete)
  in

  let rec ajout_transitivite_aux relation =
    
    print_string "APPEL DE SUPPRIME DOUBLONS AVEC RELATION = ";
    (*List.map afficher_transition ((iteration_cloture relation relation)@relation);*)
    print_newline ();
    
    let rel_temp = supprime_doublons ((iteration_cloture relation relation)@relation)
    in
    
    print_bool ((List.length (rel_temp@relation)) = (List.length relation));
    List.map afficher_transition rel_temp;
    print_string "temoin";
    print_newline ();
    
    if (List.length rel_temp) = (List.length relation)
      then begin print_string "VICTOIRE"; print_newline (); rel_temp end
      else begin ajout_transitivite_aux rel_temp end
  in
  ajout_transitivite_aux rel
  ;;

(*ajout_reflexivite renvoie la liste des transitions manquantes pour que la relation de transducteur soit reflexive*)
let ajout_reflexivite transducteur =
  
  let etats = (get_q transducteur)
  in
  let rec ajout_reflexivite_aux etats =
    print_string "QUE PASTA PUTAIN";
  print_newline ();
    match etats with
    |e::[] -> [Transition(e, "epsi", "epsi", e)]
    |e::r -> Transition(e, "epsi", "epsi", e)::(ajout_reflexivite_aux r)
  in
  ajout_reflexivite_aux etats;;


(*cloture_transitive renvoie t avec comme relation la cloture transitive de l'ancienne relation
*)
let cloture_transitive transducteur =
  print_string "APPEL DE SUPPRIME DOUBLONS DEPUIS CLOTURE TRANSITIVE";
  print_newline (); 
    let nouvelle_relation = 
      supprime_doublons ((get_relation transducteur)@(ajout_reflexivite transducteur)@(ajout_transitivite transducteur))

    in
    print_string "FIN APPEL DEPUIS CLOTURE TRANSITIVE";
    print_newline ();
    match transducteur with
    |Transducteur(alpha1, alpha2, etats, initiaux, finaux,_) -> Transducteur(alpha1, alpha2, etats, initiaux, finaux, nouvelle_relation)
  ;;

(*Instance*)
let a1 = ("a"::"b"::"c"::[]);;
let a2 = ("d"::"e"::"f"::[]);;

let q1 = "I";;
let q2 = "F";;
let q3 = "Q1";;
let q4 = "Q2";;

let q = (q1::q2::q3::q4::[]);;
let i = [q1];;
let f = [q2];;

let t1 = Transition(q1, "a", "d", q3);;
let t2 = Transition(q3, "b", "e", q2);;
let t3 = Transition(q2, "c", "f", q4);;

let relation1 = (t1::t2::t3::[]);;

let fst1 = Transducteur(a1, a2, q, i ,f, relation1);;

print_bool (test_egalite_transition t1 t1);;
print_bool (contient_transition relation1 t1);;
afficher_fst fst1;;
afficher_fst (cloture_transitive fst1);;