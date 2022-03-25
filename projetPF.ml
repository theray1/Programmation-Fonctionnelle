

(*---types---*)
type transition = {etat_deb:string; entree:string; 
                   sortie:string; etat_fin:string}
;;

type transducteur_fini = Transducteur of 
    (string list * string list *
     string list * string list * 
     string list * transition list)
;;
(* représente une étape de dérivation *)
type step = {etat:string; mot_entree:string list; mot_sortie:string list}
;;


(*---exemple 1---*)
let alphabet_entree = ["a";"b";"c"]
;; 
let alphabet_sortie = ["A";"B";"C"]
;; 
let etats1 = ["q0";"q1"]
;;
let etats_init1 = ["q0"]
;;
let etats_finaux1 = ["q1"]
;; 
let transition1 = {etat_deb="q0"; entree="a"; sortie="A"; etat_fin="q1"}
;;
let transition2 = {etat_deb="q1"; entree="b"; sortie="B"; etat_fin="q0"}
;;
let transition3 = {etat_deb="q0"; entree="a"; sortie="C"; etat_fin="q0"}
;;
let transitions1 = [transition1;transition2;transition3]
;;
let t1 = Transducteur(alphabet_entree, alphabet_sortie,
                      etats1, etats_init1, 
                      etats_finaux1, transitions1)
;;
(*---exemple 2---*) 
let etats2 = ["q2"]
;;
let etats_init2 = ["q2"]
;;
let etats_finaux2 = ["q2"]
;; 
let transition12 = {etat_deb="q2"; entree="b"; sortie="B"; etat_fin="q2"}
;;
let transition22 = {etat_deb="q2"; entree="a"; sortie="A"; etat_fin="q2"}
;; 
let transitions2 = [transition12;transition22]
;;
let t2 = Transducteur(alphabet_entree, alphabet_sortie,
                      etats2, etats_init2, 
                      etats_finaux2, transitions2)
;;

(*---exemple 3---*)
let alphabet_entree3 = ["A";"B";"C"]
;; 
let alphabet_sortie3 = ["0";"1";"2"]
;; 
let etats3 = ["q3"]
;;
let etats_init3 = ["q3"]
;;
let etats_finaux3 = ["q3"]
;; 
let transition13 = {etat_deb="q3"; entree="A"; sortie="0"; etat_fin="q3"}
;;
let transition23 = {etat_deb="q3"; entree="B"; sortie="1"; etat_fin="q3"}
;;
let transition33 = {etat_deb="q3"; entree="C"; sortie="2"; etat_fin="q3"}
;; 

let transitions3 = [transition13;transition23;transition33]
;;
let t3 = Transducteur(alphabet_entree3, alphabet_sortie3,
                      etats3, etats_init3, 
                      etats_finaux3, transitions3)
;;

(*---fonctions utiles---*)
(*TODO : fonction qui vérifie que les alphabets coincide avec les transitions *)
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
let print_transitions tra = 
  let tr_liste = (match tra with 
      | Transducteur(_,_,_,_,_,l) -> l) in
  let rec aux_affiche_transitions tra transitions = 
    match transitions with
    | [] -> print_newline ()
    | x::r -> 
        begin
          print_transition x;
          aux_affiche_transitions tra r
        end
  in aux_affiche_transitions tra tr_liste
;;



print_transitions t1
;;
print_transitions t2
;;
print_transitions t3
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
      
    print_transitions transducteur;
    end 
  ;;

let print_bool b =
  if b then print_string "true" else print_string "false";
  print_newline ();;

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

let get_alpha_entree t = match t with
  | Transducteur(ale,_,_,_,_,_) -> ale
;;
let get_alpha_sortie t = match t with
  | Transducteur(_,als,_,_,_,_) -> als
;;
let get_etats t = match t with
  | Transducteur(_,_,e,_,_,_) -> e
;;
let etats_initiaux t = match t with 
  | Transducteur(_,_,_,li,_,_) -> li
;;
let etats_finaux t = match t with 
  | Transducteur(_,_,_,_,ef,_) -> ef
;;
let get_transitions t = match t with
  | Transducteur(_,_,_,_,_,tr) -> tr
;;

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
let est_final etat list_etats_finaux = 
  List.mem etat list_etats_finaux
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
    | x::r -> aux r q symb (if(x.etat_deb=q && (x.entree=symb || x.entree="eps")) then [x] else [])@result
  in aux transitions q symb []
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
    | [] -> steps (* forme normale *)
    | x::r -> (aux t [x])@(aux t r)
  in aux t step_init
;;
final_steps t1 "abab"
;; 
final_steps t1 "aaabb"
;; 

(* retourne les sorties et un booléen *)(*OK*)
let reco t mot =
  let final_st = final_steps t mot in
  
  let rec aux t final_st result= 
    match final_st with
    | [] -> result
    | x::r -> aux t r ((list_to_string (List.rev x.mot_sortie), x.mot_entree=[]&&(est_final x.etat (etats_finaux t)))::result)
  in aux t final_st []
;;

(* clôture transitive *)

(*test l'égalité entre deux transitions r1 et r2*)
let rec test_egalite_transition r1 r2 =
  ((r1.etat_deb = r2.etat_deb) && (r1.entree = r2.entree) && (r1.sortie = r2.sortie) && (r1.etat_fin = r2.etat_fin))
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

  match relation with
  |[] -> begin [] end
  |t::r -> begin
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
    |{etat_deb = deb1; entree = ent1; sortie = sor1; etat_fin = fin1}::r ->
    begin
      (*print_bool (transition.etat_deb = deb1);*)
      if ((transition.etat_fin = deb1) && (transition.etat_deb <> deb1) && (transition.etat_fin <> fin1))
      then {etat_deb = transition.etat_deb; entree = transition.entree^ent1; sortie = transition.sortie^sor1; etat_fin = fin1}::(ajout_transitivite_depuis_une_transition transition r) 
      else (ajout_transitivite_depuis_une_transition transition r)
    end
  in

  (*appelle ajout_transitive_depuis_une_transition sur toutes les transitions de #relation*)
  let rec iteration_cloture relation_iteree relation_complete =
    match relation_iteree with
    |[] -> []
    |t::r -> begin  (ajout_transitivite_depuis_une_transition t relation_complete)@(iteration_cloture r relation_complete)
  end
  in

  let rec ajout_transitivite_aux relation =
    
    let rel_temp = supprime_doublons ((iteration_cloture relation relation)@relation)
    in
    
    if (List.length rel_temp) = (List.length relation)
      then begin rel_temp end
      else begin ajout_transitivite_aux rel_temp end
  in
  ajout_transitivite_aux rel
  ;;

(*ajout_reflexivite renvoie la liste des transitions manquantes pour que la relation de transducteur soit reflexive*)
let ajout_reflexivite transducteur =
  let etats = (get_q transducteur)
  in
  let rec ajout_reflexivite_aux etats =
    match etats with
    |e::[] -> [{etat_deb = e; entree = "epsi"; sortie = "epsi"; etat_fin = e}]
    |e::r -> {etat_deb = e; entree = "epsi"; sortie = "epsi"; etat_fin = e}::(ajout_reflexivite_aux r)
  in
  ajout_reflexivite_aux etats;;


(*cloture_transitive renvoie t avec comme relation la cloture transitive de l'ancienne relation
*)
let cloture_transitive transducteur =

    let nouvelle_relation = 
      supprime_doublons ((get_relation transducteur)@(ajout_reflexivite transducteur)@(ajout_transitivite transducteur))

    in
    
    match transducteur with
    |Transducteur(alpha1, alpha2, etats, initiaux, finaux,_) -> Transducteur(alpha1, alpha2, etats, initiaux, finaux, nouvelle_relation)
  ;;

reco t1 "abab"
reco t1 "ababa"
;;
reco t1 "aaaba"
;;

(* retourne les sortie par les chemin qui reconnaisent le mot en entier *)(*OK*)
let reco_true t mot =
  let final_st = final_steps t mot in
  
  let rec aux t final_st result= 
    match final_st with
    | [] -> result
    | x::r -> aux t r (if(x.mot_entree=[]&&(est_final x.etat (etats_finaux t))) 
                       then ((list_to_string (List.rev x.mot_sortie))::result)
                       else result)
  in aux t final_st []
;;
reco_true t1 "ababa"
;;
reco_true t1 "aaaba"
;; 

print_transitions t1;;
print_transitions (cloture_transitive t1);;
(* retourne vrai si (entree,sortie) appartient a la relation reconnu par le transducteur t *)(*OK*)
let est_en_relation t entree sortie =
  let reco_t = reco_true t entree in
  
  let rec aux reco_t sortie =
    match reco_t with
    | [] -> false
    | x::r -> (x=sortie)||(aux r sortie)
  in aux reco_t sortie
;;
est_en_relation t1 "ababa" "ABABA"
;;
est_en_relation t1 "ababa" "ABABa"
;;


(*---Concatenation de transducteurs---*) 
(* donne la liste des transitions partant de t1_final vers chaques états initiaux de t2 *)
let create_transitions_1_to_few_concat t1_final t2_initiaux =
  let rec aux t1_final t2_initiaux result =
    match t2_initiaux with
    | [] -> result
    | x::r -> aux t1_final r ({etat_deb=t1_final; entree="eps"; 
                               sortie=""; etat_fin=x}::result)
  in aux t1_final t2_initiaux []
;;

(* donne une liste de transitions entre les états finaux de t1 et les états initiaux de t2 *)
let create_transitions_concat t1_finaux t2_initiaux =
  let rec aux t1_finaux t2_initiaux result =
    match t1_finaux with 
    | [] -> result
    | x::r -> aux r t2_initiaux ((create_transitions_1_to_few_concat x t2_initiaux)@result)
  in aux t1_finaux t2_initiaux []
;; 

(* produit un transducteur qui fait la concatenation de t1 et t2*)
let concatenation tr1 tr2 =
  (*TODO:vérifier que les alhabets coincident *)
  Transducteur(get_alpha_entree tr1, 
               get_alpha_sortie tr1,
               (get_etats tr1)@(get_etats tr2), 
               etats_initiaux tr1, 
               etats_finaux tr2, 
               (create_transitions_concat (etats_finaux tr1) (etats_initiaux tr2))@(get_transitions tr1)@(get_transitions tr2))
;;

let concatt1t2 = concatenation t1 t2
;;
print_transitions concatt1t2
;;


(*---Composition de transducteurs---*)
(* a partir d'un mot execute le transducteur t1 et passe la sortie en entrée du transducteur t2 *)
let composition mot tr1 tr2 =
  (*TODO: vérifier que l'alphabet de sortie de t1 coincide avec l'alphabet d'entrée de t2*)
  let exec_t1 = reco_true tr1 mot in
  print_string (List.hd exec_t1);
  let rec compo_t2 sortie_t1 tr2 result =
    match sortie_t1 with 
    | [] -> result
    | x::r -> (compo_t2 r tr2 ((reco_true tr2 x)@result))
  in compo_t2 exec_t1 tr2 []
;;
let compot1t2 = composition "abbab" t2 t3
;; 
