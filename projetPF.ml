

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

(*---exemple 4---*)
let alphabet_entree4 = ["D";"E";"F"]
;; 
let alphabet_sortie4 = ["D";"E";"F"]
;; 
let etats4 = ["q4"]
;;
let etats_init4 = ["q4"]
;;
let etats_finaux4 = ["q4"]
;; 
let transition14 = {etat_deb="q4"; entree="D"; sortie="E"; etat_fin="q4"}
;;
let transition24 = {etat_deb="q4"; entree="E"; sortie="F"; etat_fin="q4"}
;;
let transition34 = {etat_deb="q4"; entree="F"; sortie="D"; etat_fin="q4"}
;; 

let transitions4 = [transition14;transition24;transition34]
;;
let t4 = Transducteur(alphabet_entree4, alphabet_sortie4,
                      etats4, etats_init4, 
                      etats_finaux4, transitions4)
;;

(*---fonctions utiles---*)
(*TODO : fonction qui vérifie que les alphabets coincide avec les transitions *)

let rec print_string_list l =
  begin match l with
    |e::r -> print_string e; print_string_list r
    |[] -> print_string ""
  end;
  print_newline ();
;;

let print_bool b =
  if b then print_string "true" else print_string "false";
  print_newline ();;

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

let str_comp x y = if x > y then 1 else 0;;

let rec test_egalite_alphabet a1 a2 =
  (List.sort str_comp a1) = (List.sort str_comp a2)
;;

print_bool (test_egalite_alphabet (["a"; "b"; "c"]) (["c"; "b"; "a"]));;

let afficher_fst transducteur =
  match transducteur with
  |Transducteur(a1,a2,q,i,f,r) ->
      begin
        print_string "alphabet d'entrée :";
        print_newline ();
        print_string_list a1;
        print_newline ();

        print_string "alphabet de sortie :";
        print_newline ();
        print_string_list a2;
        print_newline ();

        print_string "états :";
        print_newline ();
        print_string_list q;
        print_newline ();
      
        print_string "états initiaux :";
        print_newline ();
        print_string_list i;
        print_newline ();
      
        print_string "états finaux :";
        print_newline ();
        print_string_list f;
        print_newline ();
      
        print_transitions transducteur;
      end 
;;

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

let est_final etat list_etats_finaux = 
  List.mem etat list_etats_finaux
;;

(* supprime les doublons dans une string list CODE MORT ?*)
let suppr_doublons sl = 
  let rec aux sl result =
    match sl with
    | [] -> result
    | x::r -> aux r (if(List.mem x r) then result else x::result)
  in aux sl []
;;


(*---Verification de transducteurs---*)

(* renvoie vrai si il n'y a pas de doublons dans une string list *)
let rec no_doublons sl = 
  match sl with
  | [] -> true
  | x::r -> (not (List.mem x r))&&(no_doublons r)
;;

(*Vérifie que toutes les transitions ont pour étiquette d'entrée un élément de #alpha_entree et pour étiquette de sortie un élément de #alpha_sortie*)
let rec check_etiq_transitions alpha_entree alpha_sortie transitions = 
  match transitions with
  |[] -> true
  |t::r -> begin 
      if (List.mem t.entree alpha_entree) && (List.mem t.sortie alpha_sortie)
      then check_etiq_transitions alpha_entree alpha_sortie r
      else false
    end
;;

(*Vérifie que toutes les transitions de #transition ont pour état de début et fin des éléments de #états*)
let rec check_etats_transitions etats transitions =
  match transitions with
  |[] -> true
  |t::r -> begin
      if (List.mem t.etat_deb etats) && (List.mem t.etat_fin etats)
      then check_etats_transitions etats r
      else false 
    end
;;

(*Vérifie que les transitions de #transducteur sont bien cohérentes avec ses états et alphabet d'entée / sortie*)
let verification_transitions transducteur =
  let alpha_entree = get_alpha_entree transducteur in
  let alpha_sortie = get_alpha_sortie transducteur in
  let etats = get_etats transducteur in
  let transitions = get_transitions transducteur in
  (check_etiq_transitions alpha_entree alpha_sortie transitions)
  && (check_etats_transitions etats transitions)
;;

(*Vérifie que #transducteur a au moins un état final*)
let verification_etat_final transducteur =
  (List.length (etats_finaux transducteur)) > 0
;;

(*Vérifie que #transducteur a au moins un état initial*)
let verification_etat_initial transducteur =
  (List.length (etats_initiaux transducteur)) > 0
;;

(*Vérifie que les états finaux de #transducteur sont des éléments des états de #transducteur*)
let verification_inclusion_etat_final transducteur =
  let rec aux l =
    match l with 
    |[] -> true
    |elt::r -> (List.mem elt (get_etats transducteur)) && (aux r)
  in
  aux (etats_finaux transducteur)
;;

(*Vérifie que les états initiaux de #transducteur sont des éléments des états de #transducteur*)
let verification_inclusion_etat_initial transducteur =
  let rec aux l =
    match l with 
    |[] -> true
    |elt::r -> (List.mem elt (get_etats transducteur)) && (aux r)
  in
  aux (etats_initiaux transducteur)
;;

(*Vérifie que #transducteur est bien formé (cohérence, état initial, état final)*)
let verification_transducteur transducteur = 
  (verification_transitions transducteur) 
  && (verification_etat_final transducteur)
  && (verification_etat_initial transducteur)
  && (verification_inclusion_etat_final transducteur)
  && (verification_inclusion_etat_initial transducteur)
  && (no_doublons (get_alpha_entree transducteur))
  && (no_doublons (get_alpha_sortie transducteur))
  && (no_doublons (get_etats transducteur))
  && (no_doublons (etats_initiaux transducteur))
  && (no_doublons (etats_finaux transducteur))
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

  if (verification_transducteur t) 
  then

    let final_st = final_steps t mot in
  
    let rec aux t final_st result= 
      match final_st with
      | [] -> result
      | x::r -> aux t r ((list_to_string (List.rev x.mot_sortie), x.mot_entree=[]&&(est_final x.etat (etats_finaux t)))::result)
    in aux t final_st []

  else failwith "Le transducteur n'est pas bien formé."
;;

reco t1 "abab"
;;
reco t1 "ababa"
;;
reco t1 "aaaba"
;;

(* retourne les sortie par les chemin qui reconnaisent le mot en entier *)(*OK*)
let reco_true t mot =
  
  if (verification_transducteur t) 
  then
  
    let final_st = final_steps t mot in
  
    let rec aux t final_st result= 
      match final_st with
      | [] -> result
      | x::r -> aux t r (if(x.mot_entree=[]&&(est_final x.etat (etats_finaux t))) 
                         then ((list_to_string (List.rev x.mot_sortie))::result)
                         else result)
    in aux t final_st []
  else failwith "Le transducteur n'est pas bien formé."
;;
reco_true t1 "ababa"
;;
reco_true t1 "aaaba"
;; 

(*print_transitions t1;;*)
(*print_transitions (cloture_transitive t1);;*)
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

(*---Clôture transitive---*)

(*test l'égalité entre deux transitions r1 et r2*)
let rec test_egalite_transition r1 r2 =
  ((r1.etat_deb = r2.etat_deb) && (r1.entree = r2.entree) && (r1.sortie = r2.sortie) && (r1.etat_fin = r2.etat_fin))
;;

(*Vérifie que #transition est un élément de #relation*)
let rec contient_transition relation transition =
  match relation with
  |[] -> false
  |t::[] -> test_egalite_transition t transition
  |t::tl -> if (test_egalite_transition t transition) 
      then true 
      else (contient_transition tl transition)
;;

(*Supprime les doublons de #relation*)
let rec supprime_doublons relation =

  match relation with
  |[] -> begin [] end
  |t::r -> begin
      if contient_transition r t
      then supprime_doublons r 
      else t::(supprime_doublons r)
    end
;;

(*Ajout_transitivite renvoie la liste des transitions manquantes pour que obtenir la cloture transitive de la relation de transducteur*)
let ajout_transitivite transducteur = 
  let rel = (get_transitions transducteur) in

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

  (*Appelle ajout_transitive_depuis_une_transition sur toutes les transitions de #relation*)
  let rec iteration_cloture relation_iteree relation_complete =
    match relation_iteree with
    |[] -> []
    |t::r -> begin  (ajout_transitivite_depuis_une_transition t relation_complete)@(iteration_cloture r relation_complete)
      end
  in

  (*Retourne le plus petit ensemble de transitions reflexif contenant #relation*)
  let rec ajout_transitivite_aux relation =
    
    let rel_temp = supprime_doublons ((iteration_cloture relation relation)@relation)
    in
    
    if (List.length rel_temp) = (List.length relation)
    then begin rel_temp end
    else begin ajout_transitivite_aux rel_temp end
  in
  ajout_transitivite_aux rel
;;

(*ajout_reflexivite renvoie la liste des transitions manquantes pour que l'ensemble de transitions de #transducteur soit reflexif*)
let ajout_reflexivite transducteur =
  let etats = (get_q transducteur)
  in
  let rec ajout_reflexivite_aux etats =
    match etats with
    |[] -> []
    |e::[] -> [{etat_deb = e; entree = "eps"; sortie = ""; etat_fin = e}]
    |e::r -> {etat_deb = e; entree = "eps"; sortie = ""; etat_fin = e}::(ajout_reflexivite_aux r)
  in
  ajout_reflexivite_aux etats;;


(*cloture_transitive renvoie t avec comme ensemble de transition la cloture transitive de l'ancienne relation
*)
let cloture_transitive transducteur =

  let nouvelle_relation = 
    supprime_doublons ((get_transitions transducteur)@(ajout_reflexivite transducteur)@(ajout_transitivite transducteur))

  in
    
  match transducteur with
  |Transducteur(alpha1, alpha2, etats, initiaux, finaux,_) -> Transducteur(alpha1, alpha2, etats, initiaux, finaux, nouvelle_relation)
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
(*print_transitions concatt1t2*)
;;


(*---Composition de transducteurs---*)
(* a partir d'un mot execute le transducteur t1 et passe la sortie en entrée du transducteur t2 *)
let composition mot tr1 tr2 =
  if (test_egalite_alphabet (get_alpha_sortie tr1) (get_alpha_entree tr2))
  then 
    let exec_t1 = reco_true tr1 mot in
    let rec compo_t2 sortie_t1 tr2 result =
      match sortie_t1 with 
      | [] -> result
      | x::r -> (compo_t2 r tr2 ((reco_true tr2 x)@result))
    in compo_t2 exec_t1 tr2 []
  else failwith "output alphabet and input alphabet do not match"
;;
let compot1t2 = composition "abbab" t2 t3
;;
(*print_string_list compot1t2*)
;; 


(*---Mise à l'étoie de Kleene---*)
let etoile_kleene transducteur = 
  Transducteur((get_alpha_entree transducteur),
               (get_alpha_sortie transducteur),
               (get_etats transducteur),
               (etats_finaux transducteur),
               (etats_initiaux transducteur),
               ((get_transitions transducteur)@(create_transitions_concat (etats_finaux transducteur) (etats_initiaux transducteur))))

;;

let test = etoile_kleene t1;;

afficher_fst test;;

print_bool (verification_transitions t1);; 
