type 'a mot_t = 'a list

type etat_t = int


type 'a t = {
	nb: int; (* nombre d'états : numérotés 0, 1, ..., nb-1 *)
	sigma: 'a array;
	i: etat_t list;
	f: etat_t list;
	delta: (etat_t * 'a, etat_t list) Hashtbl.t (*transitions: on associe un état + une lettre à une liste d'états d'arrivée (car pas forcément déterministe)*)
}


let ajouter_transition_delta delta q1 lettre q2 : unit =
  match Hashtbl.find_opt delta (q1,lettre) with
  | None -> Hashtbl.add delta (q1,lettre) [q2]
  | Some(l) -> if not (List.mem q2 l) then Hashtbl.replace delta (q1, lettre) (q2::l) (*ajoute q2 à la liste d'arrivée si la lettre existait déjà pour q1*)
;;


let ajouter_transition (a: 'a t) (q1: etat_t) (lettre: 'a) (q2: etat_t) : unit =
  assert(q1 >= 0 && q1 < a.nb);
  assert(q2 >= 0 && q2 < a.nb);
  assert(Array.mem lettre a.sigma);
  ajouter_transition_delta a.delta q1 lettre q2
;;


let retirer_transition_delta delta q1 lettre q2 : unit =
  let rec retirer_elt_liste l elt =
    match l with
    | [] -> raise Not_found
    | h::t -> if h = elt then t else retirer_elt_liste t elt
  in

  match Hashtbl.find_opt delta (q1,lettre) with
  | None -> raise Not_found
  | Some(l) -> (
    let new_l = retirer_elt_liste l q2 in
    if new_l = []
    then Hashtbl.remove delta (q1, lettre)
    else Hashtbl.replace delta (q1, lettre) new_l
  )
;;


let retirer_transition (a: 'a t) (q1: etat_t) (lettre: 'a) (q2: etat_t) : unit =
  retirer_transition_delta a.delta q1 lettre q2
;;


let affiche_transitions (a : 'a t) (print_lettre : 'a -> unit) : unit =
  for i = 0 to a.nb - 1 do
    Printf.printf "Etat %i: \n" i;
    for j = 0 to (Array.length a.sigma) - 1 do
      match Hashtbl.find_opt a.delta (i, a.sigma.(j)) with
      | None -> ()
      | Some(l) ->
        print_string "    ";
        Printf.printf "[%i] -> " i;
        print_lettre a.sigma.(j);
        print_string " -> [";
        List.iter (fun x -> print_int x; print_string " ") l;
        print_string "]";
    print_newline ()
    done
  done
;;

let affiche (a: 'a t) (print_lettre: 'a -> unit) : unit =
  Printf.printf "Nombre d'états: %d\n" a.nb;
  print_string "I : ";
  List.iter (fun x -> print_int x ; print_string " ") a.i ;
  print_newline ();
  print_string "F : ";
  List.iter (fun x -> print_int x ; print_string " ") a.f ;
  print_newline ();
  affiche_transitions a print_lettre;
  print_newline ()
;;


let affiche_char (a : 'a t) : unit = affiche a print_char;;

(* O(|I| + |delta|) *)
let est_deterministe (a: 'a t) : bool =
  if List.length a.i > 1 
  then false
  else Hashtbl.fold (fun _key value acc -> (List.length value <= 1) && acc) a.delta true
;;

(*IMPLEMENTATION DES ENSEMBLES
Un ensemble est un tableau de booléens, i appartient ou n'appartient pas selon la valeur de la i-ième case*)

type ensemble = bool array;;

let appartient (i: int) (p: ensemble) = p.(i);;
let vide (n: int) : ensemble = Array.make n false;;
let est_vide (p: ensemble) : bool = not (Array.exists (fun x -> x = true) p);;
(*Alloue un nouveau tableau pour l'intersection*)
let intersection_parties (p1: ensemble) (p2: ensemble) : ensemble = Array.map2 (fun e1 e2 -> e1 && e2) p1 p2;;
(*Alloue un nouveau tableau pour l'union*)
let union_parties (p1: ensemble) (p2: ensemble) : ensemble = Array.map2 (fun e1 e2 -> e1 || e2) p1 p2;;

let partie_vers_liste (p: ensemble) = fst (Array.fold_left (fun (acc, i) x -> if x then (i::acc, i+1) else (acc, i+1)) ([], 0) p);;

(*Ajoute les elts de p2 à p1*)
let ajoute_partie (p1: ensemble) (p2: ensemble) : unit = 
  for i = 0 to (Array.length p1) - 1 do
      p1.(i) <- p1.(i) || p2.(i)
  done
;;

(*Construit un tableau de taille n contenant les elements de la liste l*)
let construit_partie (l: int list) (n: int) : ensemble =
  let a: ensemble = vide n in
  List.iter (fun elt -> a.(elt) <- true) l;
  a
;;

(*Renvoie l'ensemble les états atteint en lisant la lettre b depuis q*)
let lire_lettre_etat (a: 'a t) (q: etat_t) (b: 'a) : ensemble =
  match Hashtbl.find_opt a.delta (q, b) with
  | None -> vide a.nb
  | Some(p) -> construit_partie p a.nb
;;

(*Renvoie l'ensemble des états atteint en lisant la lettre b depuis les états de p*)
let lire_lettre_partie (a: 'a t) (p: ensemble) (b: 'a) : ensemble =
  (*On fait l'union des lire_lettre_etat sur chaque état de la partie p*)
  let acc = vide a.nb in
  Array.iteri (fun i elt -> if elt then ajoute_partie acc (lire_lettre_etat a i b)) p;
  acc
;;

let rec lire_mot (a: 'a t) (p: ensemble)  (u: 'a mot_t) : ensemble =
  (*u est une liste de lettres. Pour chaque lettre de u, on avance, on a une nouvelle partie d'états. On renvoie la partie finale.*)
  let aux u (acc: ensemble) : ensemble =
    match u with
    | [] -> acc
    | h::t -> let acc = lire_lettre_partie a p h in
      if est_vide acc then vide a.nb else lire_mot a acc t (*on sort dès que l'automate ne peut plus rien lire*)
  in
  aux u p
;;

let accepte_mot a u =
  (*Vérifie que l'intersection entre les états atteints en lisant u depuis les états initiaux de l'automate, et les états acceptants, est non-vide.*)
  let etats_finaux = lire_mot a (construit_partie a.i a.nb) u in
  let etats_acceptants = construit_partie a.f a.nb in
  not (est_vide (intersection_parties etats_finaux etats_acceptants))
;;

(*Calcule le nombre associé à un ensemble lu en binaire*)
(*Permet de créer une bijection entre les ensembles à n éléments et [0; 2^n - 1] pour créer les super-états*)
let ensemble_vers_entier (p: ensemble) : int =
  if p = [||] then 0 else
  Array.fold_left (fun acc x -> if x then acc*2 + 1 else acc*2) 0 p
;;

(*p: entier à convertir, fournira un ensemble de max n éléments - aucun check n'est fait là-dessus*)
let entier_vers_ensemble (p: int) (n: int) : ensemble =
  let arr: ensemble = vide n in
  let rec aux (p: int) (i: int) : unit =
      (*On vérifie lorsque p = 0 que n était exactement de la bonne taille pour stocker p*)
      if p = 0 then ()
      else begin
          if p mod 2 = 1 then arr.(i) <- true;
          aux (p/2) (i-1)
      end
  in
  aux p (n-1);
  arr
;;

(*Calcule p^n*)
let puissance (p: int) (n: int) =
  (*Exponentiation rapide*)
  let rec aux p n acc =
      if n = 0 then acc
      else if n mod 2 = 0 then aux (p*p) (n / 2) acc
      else aux p (n - 1) (acc * p)
  in
  aux p n 1
;;

let est_non_disjoint (p1: ensemble) (p2: ensemble) : bool =
  not (est_vide (intersection_parties p1 p2))
;;


(*Crée une table d'association "ancien_etat -> etat_renommé" pour ramener les numéros des états entre 0 et n-1 (n: nb d'états non-vides). Renvoie la table et n.*)
let etats_non_vides (a: 'a t) : (etat_t, int) Hashtbl.t * etat_t list * etat_t list * int =
  let res: int ref = ref 0 in
  let i = ref [] in
  let f = ref [] in
  let table = Hashtbl.create a.nb in
  List.iter (fun etat ->
    if not (Hashtbl.mem table etat) then (
      Hashtbl.add table etat !res;
      i := (etat::!i);
      if List.mem etat a.f then f := (etat::!f);
      incr res
    )
  ) a.i;
  Hashtbl.iter (fun (etat_depart, lettre) lst_etats ->
    if not (Hashtbl.mem table etat_depart) then (
      Hashtbl.add table etat_depart !res;
      incr res
    );
    List.iter (fun etat' -> if not (Hashtbl.mem table etat') then (
      Hashtbl.add table etat' !res;
      if List.mem etat' a.f && not (List.mem etat' !f) then f := (etat'::!f);
      incr res)
    ) lst_etats
  ) a.delta;

  (table, !i, !f, !res)
;;
  
  
(*Renomme les états de l'automate pour les ramener dans [0, n-1] grâce à etats_non_vides.*)
let renomme (a: 'a t) : 'a t =
  let (table, i_sans_vide, f_sans_vide, nb_sans_vide) = etats_non_vides a in (*renvoie une table ancien_etat -> etat_renommé*)
  let delta_sans_vide = Hashtbl.create nb_sans_vide in
  Hashtbl.iter (fun (etat_depart, lettre) lst_etats ->
    let etat_depart_id = Hashtbl.find table etat_depart in
    List.iter (fun etat' ->
      let etat'_id = Hashtbl.find table etat' in
      ajouter_transition_delta delta_sans_vide etat_depart_id lettre etat'_id
    ) lst_etats
  ) a.delta;

  {
    nb = nb_sans_vide;
    sigma = a.sigma;
    i = i_sans_vide;
    f = f_sans_vide;
    delta = delta_sans_vide;
  }
;;


(*AGORITHME DÉTERMINISATIOBN
on part de l'état initial qu'on met dans la pile en tant que super-état.
Tant que la pile n'est pas vide: On prend un état.
	Pour chaque lettre:
		On lit la lettre depuis l'état, ça donne new_etat.
		si c'est un nouvel état: on l'ajoute à la pile et aux états de A'.
		on insère la transition de état à new_état en lisant la lettre dans delta.
On construit maintenant les états finaux:
	Pour chaque super-état construit dans A':
		s'il existe un état de A qui était final dans ce super-état de A',
		alors on marque ce super-état comme final dans A'.
On renvoie l'automate finalement construit.*)
let determinise_non_renomme (a: 'a t) : 'a t =
  if est_deterministe a then a else (*On vérifie avant tout que ce soit bien nécessaire de déterminiser, c'est moins couteux*)
  let n, taille_sigma = a.nb, Array.length a.sigma in
  let vus = Array.make (puissance 2 n) false in (*tableau des super-états déjà créés/visités*)
  let est_etat_final = est_non_disjoint (construit_partie a.f n) in (*application partielle: renvoie si un état contient un état final*)
  
  (*Construction a'*)
  let etat_finaux = ref [] in
  let delta': (etat_t * 'a, etat_t list) Hashtbl.t = Hashtbl.create (n * n) in (*transitions du déterminisé*)
  let i' = construit_partie a.i n in
  vus.(ensemble_vers_entier i') <- true;

  let rec process (pile: ensemble list) =
    match pile with
    | [] -> ()
    | etat::tail -> (
      let nb_etat = ensemble_vers_entier etat in
      if est_etat_final etat then etat_finaux := etat::!etat_finaux;
      let nouveaux_voisins = ref [] in
      for i = 0 to taille_sigma - 1 do 
        (*Lecture de la lettre courante `i` depuis le super-état `etat`*)         
        let new_etat = lire_lettre_partie a etat a.sigma.(i) in
        let nb_new_etat = ensemble_vers_entier new_etat in

        (*Ajout de la transition de super-états*)
        Hashtbl.add delta' (nb_etat, a.sigma.(i)) [nb_new_etat];
        
        (*Ajout à la pile si c'est un nouveau super-état*)
        if not vus.(nb_new_etat) then (
          nouveaux_voisins := new_etat::!nouveaux_voisins;
          vus.(nb_new_etat) <- true;
        )
      done;
      process (!nouveaux_voisins @ tail)
    )
  in process [i'];

  (*On renvoie finalement l'automate*)
  {
    nb = puissance 2 n; (*Exponentiel en |états| en espace -> on le réduit juste après.*)
    sigma = a.sigma;
    i = [ensemble_vers_entier i'];
    f = List.map (fun etat -> ensemble_vers_entier etat) !etat_finaux;
    delta = delta'
  }
;;


(*Déterminise en enlevant les états vides*)
let determinise a = renomme (determinise_non_renomme a);;


(*Calcule l'automate union de a1 et a2. Prérequis: mêmes alphabets.*)
let union (a1: 'a t) (a2: 'a t) =
  (*Fonction auxiliaire pour renommer les états du 2nd automate au-delà des états du 1er automate*)
  let rec rajoute_n_liste lst acc =
    match lst with
    | [] -> List.rev acc
    | h::t -> rajoute_n_liste t ((h + a1.nb)::acc) in

    
  (*Ajoute les transitions de a1 et a2 à delta*)
  let delta = Hashtbl.create (Hashtbl.length a1.delta + Hashtbl.length a2.delta) in
  Hashtbl.iter (fun (etat_depart, lettre) lst_etats ->
    Hashtbl.add delta (etat_depart, lettre) lst_etats
  ) a1.delta;

  Hashtbl.iter (fun (etat_depart, lettre) lst_etats ->
    Hashtbl.add delta (etat_depart + a1.nb, lettre) (rajoute_n_liste lst_etats [])
  ) a2.delta;

  {
    nb = a1.nb + a2.nb;
    sigma = a1.sigma;
    i = a1.i @ (rajoute_n_liste a2.i []) ;
    f = a1.f @ (rajoute_n_liste a2.f []);
    delta = delta;
  }
;;


(*Construit le produit cartésien (en renommant les états pour que ce soient des ints) en partant de 2 listes d'états. n1 est le nb total d'états du 1er automate.*)
let etats_prod (e1: etat_t list) (e2: etat_t list) (n1: int) : etat_t list =
  let rec aux (x: etat_t) (acc: etat_t list) (e1: etat_t list) : etat_t list =
    match e1 with
    | [] -> acc
    | h::t -> aux x ((x * n1 + h)::acc) t
  in 
  List.fold_left (fun (acc: etat_t list) (x: etat_t) ->  aux x acc e1) [] e2
;;


(*Calcule l'automate intersection de a1 et a2. Prérequis: mêmes alphabets.*)
let intersection_non_renomme (a1: 'a t) (a2: 'a t) =
  if a1.nb = 0 then a2 else if a2.nb = 0 then a1 else
  let taille_sigma = Array.length a1.sigma in
  let vus = Array.make (a1.nb * a2.nb) false in (*tableau des états du produit déjà créés/visités*)
  let idx_vers_a x1 x2 = x1*a2.nb + x2 in (*fonction pour trouver l'indice d'un état du produit*)
  let idx_depuis_a x = (x / a2.nb, x mod a2.nb) in (*fonction pour retrouver les états initiaux correspondant à l'état produit x*)
  let etats_init = etats_prod a1.i a2.i a1.nb in
  let etats_finaux = etats_prod a1.f a2.f a1.nb in
  let delta = Hashtbl.create a1.nb in (*transitions du produit*)

  (*Construction des transitions*)
  List.iter (fun x -> vus.(x) <- true) etats_init;
  List.iter (fun x -> vus.(x) <- true) etats_finaux;

  let rec process (pile: etat_t list) =
    match pile with
    | [] -> ()
    | x::tail -> (
      let nouveaux_voisins = ref [] in
      let (x1, x2) = idx_depuis_a x in
      for i = 0 to taille_sigma - 1 do
        let lettre = a1.sigma.(i) in
        (*Lecture de la lettre courante `i` depuis le super-état `etat`*)
        match Hashtbl.find_opt a1.delta (x1, lettre), Hashtbl.find_opt a2.delta (x2, lettre) with
        | None, _ | _, None -> ()
        | Some(etats_arrivee1), Some(etats_arrivee2) -> (
          (*Construction préalable de tous les états d'arrivée vers lesquels on doit ajouter une transition*)
          let a_ajouter = ref [] in
          List.iter (fun a ->
            List.iter (fun b ->
              let c = idx_vers_a a b in
              a_ajouter := c::!a_ajouter;
              if not vus.(c) then (
                let _ = Printf.printf "nouveau voisin: %d (%d, %d)\n" c a b in
                nouveaux_voisins := c::!nouveaux_voisins;
                vus.(c) <- true;
              )
            ) etats_arrivee2
          ) etats_arrivee1;

          (*Ajout des transitions*)
          List.iter (fun a -> ajouter_transition_delta delta x lettre a) !a_ajouter;
        );
      done;
      process (!nouveaux_voisins @ tail)
    )
  in process etats_init;
    
  (*On renvoie finalement l'automate*)
  {
    nb = a1.nb * a2.nb; (*Exponentiel en |états| en espace -> on le réduit juste après.*)
    sigma = a1.sigma;
    i = etats_init;
    f = etats_finaux;
    delta = delta
    }
;;
    
    
let intersection a1 a2 = renomme (intersection_non_renomme a1 a2);;
    

(*Renvoie l'automate concaténé de a1 et a2*)
let concatenation (a1: 'a t)  (a2: 'a t) : 'a t =
  let taille_sigma = Array.length a1.sigma in
  let idx_a2_vers_a q = a1.nb + q in
  let nb = a1.nb + a2.nb in

  (*Création d'ensembles pour ces listes, utile car on a besoin de faire des intersections/unions.*)
  let init_a1   : ensemble = Array.make nb false in 
  let init_a2   : ensemble = Array.make nb false in 
  let finaux_a1 : ensemble = Array.make nb false in 
  let finaux_a2 : ensemble = Array.make nb false in 
  List.iter (fun x -> init_a1.(x) <- true) a1.i;
  List.iter (fun x -> finaux_a1.(x) <- true) a1.f;
  List.iter (fun x -> init_a2.(idx_a2_vers_a x) <- true) a2.i;
  List.iter (fun x -> finaux_a2.(idx_a2_vers_a x) <- true) a2.f;

  (*Ensuite on ajoute des états finaux/initiaux si besoin :
  - Si l'intersection entre a1.f et a1.i est non-nulle alors tous les a2.i deviennent finaux dans a
  - Si l'intersection entre a2.f et a2.i est non-nulle alors tous les a1.f deviennent finaux dans a*)
  let etats_init =
    let partie_init = if est_non_disjoint init_a1 finaux_a1 then union_parties init_a1 init_a2 else init_a1 in
    partie_vers_liste partie_init
  in
  let etats_finaux =
    let partie_finaux = if est_non_disjoint init_a2 finaux_a2 then union_parties finaux_a1 finaux_a2 else finaux_a2 in
    partie_vers_liste partie_finaux
  in

  (*Pour les transitions: d'abord on copie les transitions des 2 dans une seule Hashtbl.*)
  let delta = Hashtbl.copy a1.delta in
  Hashtbl.iter (fun (etat, lettre) liste_etats -> (*Ajout de toutes les transitions de a2.delta à delta*)
    Hashtbl.add delta (idx_a2_vers_a etat, lettre) (List.map idx_a2_vers_a liste_etats)
  ) a2.delta;
  
  (*Ensuite on fait le lien entre les 2 automates.*)
  List.iter (fun etatf ->
    List.iter (fun etati ->
      for k = 0 to taille_sigma - 1 do
        let lettre = a1.sigma.(k) in
        match Hashtbl.find_opt a2.delta (etati, lettre) with
        | None -> ()
        | Some(liste_etats) -> Hashtbl.add delta (etatf, lettre) (List.map idx_a2_vers_a liste_etats)
      done
    ) a2.i
  ) a1.f;

  {
    nb = nb;
    sigma = a1.sigma;
    i = etats_init;
    f = etats_finaux;
    delta = delta;
  }
;;





let tests () =
  Printexc.record_backtrace true;
  (*Exemple : automate a0*)
  let (a0: char t) = {
    nb = 6;
    sigma = [|'a';'b'|];
    i = [0];
    f = [1;4];
    delta = Hashtbl.create 36;
  } in
  ajouter_transition a0 0 'a' 1;
  ajouter_transition a0 0 'b' 0;
  ajouter_transition a0 1 'a' 3;
  ajouter_transition a0 1 'b' 2;
  ajouter_transition a0 2 'a' 5;
  ajouter_transition a0 2 'b' 1;
  ajouter_transition a0 3 'a' 4;
  ajouter_transition a0 3 'b' 0;
  ajouter_transition a0 4 'a' 0;
  ajouter_transition a0 4 'b' 5;
  ajouter_transition a0 5 'a' 5;
  ajouter_transition a0 5 'b' 1;

  (*Deuxième automate test*)
  let (a1: char t) = {
    nb = 4 ;
    sigma = [|'a';'b'|] ;
    i = [0; 1] ;
    f = [2] ;
    delta = Hashtbl.create 12
  } in
  
  ajouter_transition a1 0 'b' 1;
  ajouter_transition a1 0 'b' 2;
  ajouter_transition a1 1 'a' 2;
  ajouter_transition a1 1 'b' 3;
  ajouter_transition a1 2 'a' 0;
  ajouter_transition a1 2 'a' 2;
  ajouter_transition a1 2 'b' 3;
  ajouter_transition a1 3 'b' 1;
  ajouter_transition a1 3 'b' 2;

  (*Automate reconnaissant les mots ayant un a en dernière position*)
  let (a2: char t) = { 
    nb = 2;
    sigma = [|'a';'b'|] ;
    i = [0] ;
    f = [1] ;
    delta = Hashtbl.create 12
  } in
  ajouter_transition a2 0 'a' 0;
  ajouter_transition a2 0 'b' 0;
  ajouter_transition a2 0 'a' 1;

  (*Automate reconnaissant les mots ayant un a en n-3ème position*)
  let (a3: char t) = {
    nb = 4;
    sigma = [|'a';'b'|] ;
    i = [0] ;
    f = [3] ;
    delta = Hashtbl.create 12
  } in
  ajouter_transition a3 0 'a' 0;
  ajouter_transition a3 0 'b' 0;
  ajouter_transition a3 0 'a' 1;
  ajouter_transition a3 1 'a' 2;
  ajouter_transition a3 1 'b' 2;
  ajouter_transition a3 2 'a' 3;
  ajouter_transition a3 2 'b' 3;

  assert (ensemble_vers_entier [|false; false; true|] = 1);
  assert (ensemble_vers_entier [|true; false; false|] = 4);
  assert (ensemble_vers_entier [|true; false; true|] = 5);
  assert (ensemble_vers_entier [|true; true; true|] = 7);

  assert (entier_vers_ensemble 1 3 = [|false; false; true|]);
  assert (entier_vers_ensemble 4 3 = [|true; false; false|]);
  assert (entier_vers_ensemble 5 3 = [|true; false; true|]);
  assert (entier_vers_ensemble 7 3 = [|true; true; true|]);

  assert (puissance 2 0 = 1);
  assert (puissance 2 1 = 2);
  assert (puissance 2 2 = 4);
  assert (puissance 2 3 = 8);
  assert (puissance 2 4 = 16);

  assert (partie_vers_liste [|true; true; true; true|] = [3; 2; 1; 0]);
  assert (partie_vers_liste [|false; false; false; false|] = []);
  assert (partie_vers_liste [|true; false; false; false|] = [0]);
  assert (partie_vers_liste [|false; false; true; true|] = [3; 2]);

  assert (est_deterministe a0);
  assert (not (est_deterministe a1));
  assert (not (est_deterministe a2));
  assert (not (est_deterministe a3));

  assert (est_deterministe (determinise a0));
  assert (est_deterministe (determinise a1));  
  assert (est_deterministe (determinise a2));
  assert (est_deterministe (determinise a3));


  (*TEST DETERMINISE*)
  (* print_endline "Automate a3 ---------------------------";
  affiche_char a3;
  print_endline "Automate a3 déterminisé ---------------------------";
  affiche_char (determinise a3);
  print_endline "Automate a3 déterminisé renommé ---------------------------";
  affiche_char (renomme (determinise a3)); *)

  (*TEST UNION*) (*TODO vérifier avec graphviz*)
  (* print_endline "Automate a0 ---------------------------";
  affiche_char a0;
  print_endline "Automate a1 ---------------------------";
  affiche_char a1; 
  print_endline "Automate union de a0 et a1 ---------------------------";
  affiche_char (union a0 a1);
  print_endline "Automate union de a0 et a1 déterminisé ---------------------------";
  affiche_char (determinise (union a0 a1)) *)


  (*TEST INTERSECTION*)
  (* print_endline "Automate a2 ---------------------------";
  affiche_char a2;
  print_endline "Automate a3 ---------------------------";
  affiche_char a3;
  print_endline "Automate intersection de a2 et a3 ---------------------------"; (*a2: A*a, a3: A*aAA, donc a2 inter a3: A* aAa*)
  affiche_char (intersection a2 a3);  *)

  (*TEST CONCATENATION*)
  (* print_endline "Automate a2 ---------------------------";
  affiche_char a2;
  print_endline "Automate a3 ---------------------------";
  affiche_char a3;
  print_endline "Automate concaténation de a2 et a3 ---------------------------";
  affiche_char (concatenation a2 a3) *)

;;

tests ();;