type 'a mot = 'a list
type etat = int
type 'a automate = {
  nb : etat;
  sigma : 'a array;
  i : etat mot;
  f : etat mot;
  delta : (etat * 'a, etat mot) Hashtbl.t;
}


(**Affiche les différentes composantes et transitions de l'automate d'entrée à l'aide de la fonction d'affichage donne en entrée.*)
val affiche_automate : 'a automate -> ('a -> unit) -> unit

(**Affiche un automate ayant un alphabet de caractères en terminal*)
val affiche_automate_char : char automate -> unit

(** Ramène les états de l'automate sur un intervalle en enlevant les états vides*)
val renomme : 'a automate -> 'a automate

(**Renvoie un nouvel automate ayant la transition (q, a, q') ajoutée*)
val ajouter_transition : 'a automate -> etat -> 'a -> etat -> unit

(**Renvoie true ou false selon si l'automate est déterministe ou non, i.e si max. 1 état initial et
    max 1 état d'arrivée dans les transitions par couple (état, lettre)*)
val est_deterministe : 'a automate -> bool

(**Renvoie true ou false selon si l'automate accepte le mot donné*)
val accepte_mot : 'a automate -> 'a mot -> bool

(**Renvoie une version déterministe de l'automate d'entrée*)
val determinise : 'a automate -> 'a automate

(**Renvoie un automate représentant l'union des 2 automates d'entrée*)
val union_automates : 'a automate -> 'a automate -> 'a automate

(**Renvoie un automate représentant l'intersection des 2 automates d'entrée*)
val intersection_automates : 'a automate -> 'a automate -> 'a automate