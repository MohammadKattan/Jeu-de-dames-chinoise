let dim = 3;;
type case = int * int * int;;
type couleur =
  | Vert
  | Jaune
  | Rouge
  | Noir
  | Bleu
  | Marron
  | Libre (*case libre du plateau*)
  | Dehors
  (*case en dehors du plateau, utile pour l'affichage*)
  | Nombre of int
  (*pour mettre des petits noms*)
  | Nom of string;;
let string_of_couleur x =
  match x with
  | Vert -> "Vert"
  | Jaune -> "Jaune"
  | Rouge -> "Rouge"
  | Noir -> "Noir"
  | Bleu -> "Bleu"
  | Marron -> "Marron"
  | Libre -> "Libre"
  | Dehors -> "Dehors"
  | Nombre n -> string_of_int n
  (*pour mettre des petits noms*)
  | Nom s -> s;;
type case_coloree = case * couleur;;
type configuration = case_coloree list * couleur list;;
type coup = Du of case * case | Sm of case list;;
let configuration_initial = ([], [ Vert; Jaune; Rouge;]);;
let liste_joueurs (_, l) = l;;
(* let mis_a_jour_configuration _ _ = Error "To do";; *)
let gagnant _ = Libre
(*Q2*)
let est_dans_losange = fun ((i,j,k):case)->
  i >= -2*dim && i <= 2*dim && j >= -dim && j <= dim && k >= -dim &&k <= dim;;
(*Q3*)
let est_dans_etoile = fun ((i,j,k):case)->
  (i+j+k=0)&&(
    (i >= -2*dim && i <= 2*dim && j >= -dim && j <= dim && k >= -dim &&k <= dim)
    ||(i >= -dim && i <= dim &&j >= -2*dim && j <= 2*dim && k >= -dim &&k <= dim)
    ||(i >= -dim && i <= dim && j >= -dim && j <= dim && k >= -2*dim && k <= 2*dim)) ;;
(*Q5*)
let rec tourner_case = fun m-> fun ((i,j,k):case) -> 
  if m = 0 then ((i,j,k):case)
  else tourner_case (m-1)((-k,-i,-j):case);;
(*Q6*)
let rec tourne_coul_aux liste_coul coul = 
  match liste_coul with 
    []-> coul::[]
  |t::q -> t::(tourne_coul_aux q coul);;
let tourne_couleurs liste_couleurs = match liste_couleurs with 
    []-> []
  |t ::q -> tourne_coul_aux q t ;;
let rec tourne_case_coul liste_case_coul = match liste_case_coul with 
    []-> []
  | t::q-> (match t with 
      |(k,coul)->((tourner_case 2 k),coul)::(tourne_case_coul q))
let tourne_config = fun ((conf):configuration) ->
  match conf with 
    ([],[])->[],[]
  |(case,coul)->((tourne_case_coul case ,tourne_couleurs coul):configuration);;
(*Q7*)
let sont_voisines = fun (c1:case) (c2:case)->
  if est_dans_etoile c1 && est_dans_etoile c2 then 
    let (i,j,k)=c1 in 
    let (x,y,z)=c2 in 
    ((i,j,k) = (x,y+1,z-1))||((i,j,k) = (x,y-1,z+1))
    ||((i,j,k)= (x-1,y,z+1))||((i,j,k)= (x+1,y,z-1))
    ||((i,j,k)= (x-1,y+1,z))||((i,j,k)= (x+1,y-1,z))
  else false ;;
(* Q8 *) 
let rec case_dans_config ((c):case) (conf:configuration) = let (i,j,k)= c in 
  if est_dans_etoile c then match conf with
      (case_co_list,color_list)->  List.mem_assoc (i,j,k) case_co_list
  else false;;
(* 9 *)
let rec  quelle_couleur (i,j,k:case) (conf:configuration) =
  if (est_dans_etoile (i,j,k) = false )then Dehors
  else  let (liste_pions,liste_couleur) = conf in 
    match liste_pions with 
      []-> Libre
    |  t::q -> let (case,coul) = t in 
      if case = (i,j,k) then coul
      else quelle_couleur (i,j,k) (q,liste_couleur)   ;; 
(*Q10*)
let case_suivante (c:case) = match c with 
  | (i,j,k) when i=(-dim)*2-> (i+1,j,k-1)
  |(i,j,k) when i=((-dim)*2)+1 && k=dim-1 -> (i,j-1,k+1)
  |(i,j,k) when i=((-dim)*2)+1 && j=dim-1 -> (i+1,j+1,k-2)
  |(i,j,k) when i=((-dim)*2)+2 && k=dim-2 -> (i,j-1,k+1)
  |(i,j,k) when i=((-dim)*2)+2 && k=dim-1 -> (i,j-1,k+1)
  | _ -> (0,0,0);;
let rec remplir_triangle (config : configuration) col c = 
  match config with 
    (case_col_list,col_list) -> let (i,j,k)=c in if (abs (i)+ abs (j)+abs (k)) < 8  then config else 
      remplir_triangle ((c,col)::case_col_list,col_list) col (case_suivante c);;
(*Q11*)
let rec remplir (config:configuration) list_jo  = match list_jo with
  |[] -> config
  |t :: q -> remplir (tourne_config (remplir_triangle config t ((-dim*2),dim,dim))) q ;;
let remplir_init list_jo = 
  let conf = ([],list_jo) in remplir conf list_jo;;
let  configuration_initial = remplir_init [ Vert; Jaune; Rouge ] ;;
(*Q12*)
let est_dep_unit config c1 c2 = 
  let (case_col_list , col_list ) = config in match col_list with 
    []-> false
  |t::q -> let col =(quelle_couleur c1 config)in 
    if col!=Libre 
    && col=t 
    && sont_voisines c1 c2 
    && (quelle_couleur c2 config)=Libre 
    && est_dans_losange c2 
    then true 
    else false ;;
(* 13 *)
(* deuxiéme méthode  *)
let fait_dep_unit config c1 c2 =
  let (case_col_list,col_list)= config in 
  match col_list with 
    []-> config
  |t:: q -> (((c2,t)::List.remove_assoc c1 case_col_list,col_list):configuration) ;;
(* 14 *)
(* let mis_a_jour_configuration config cp = 
   match cp with 
    (Du(c1,c2))->if est_dep_unit config c1 c2=false  then Error "ce n'est pas un déplacement unitaire" else
      let novelle_config = tourne_config (fait_dep_unit config c1 c2) in
      Ok novelle_config
   | (Sm([]))-> Error "Selctioner au moins deux cases"
   |(Sm([c1]))->Error "Selctioner au moins deux cases"
   |(Sm(c1::c2::q))->if est_dep_unit config c1 c2=false  then Error "ce n'est pas un déplacement unitaire" else
      let novelle_config = tourne_config (fait_dep_unit config c1 c2) in
      Ok novelle_config;; *)
(* Partie deux *)
(* 15 *)
let add_case (c1:case) (c2:case) = let (i,j,k)=c1 in let (a,b,c)=c2 in ((i+a,j+b,k+c):case) ;;
let diff_case (c1:case) (c2:case) = let (i,j,k)=c1 in let (a,b,c)=c2 in ((i-a,j-b,k-c):case) ;;
(* 16 *)
let cases_alignees (c1:case) (c2:case) = let (i,j,k)=c1 in let (a,b,c) = c2 in  i = a || j = b || k = c ;;
let case_pivot (c:case) = let (i,j,k)=c in ((i/2,j/2,k/2):case);;
let calcul_pivot (c1:case) (c2:case) = 
  if cases_alignees c1 c2 = false then None 
  else 
    let (i,j,k)=c1 in let (a,b,c)=c2 in
    let n = if i = a then  j+b
      else if j = b then i+a 
      else j+b in 
    if abs (n) mod 2 = 0 then Some (case_pivot (add_case c1 c2))
    else None ;;
(* 17 *)
let vec_et_dist c1 c2 = 
  let diff = diff_case c1 c2 in 
  let (i,j,k) = diff in 
  if i=0 then (((0,j/abs(j),k/abs(k)):case),abs(j))
  else if j=0 then ((i/abs(i),0,k/abs(k)),abs(i))
  else ((i/abs(i),j/abs(j),0),abs(i));;
(* 18 *)
let rec est_libre_seg_aux c1 config vec  = 
  let (v,d) = vec in
  if d=1  then true   
  else 
    let suiv = diff_case c1 v in 
    if case_dans_config suiv config = false  then est_libre_seg_aux  suiv  config (v,d-1)
    else false ;;
let est_libre_seg c1 c2 config = 
  let vec = vec_et_dist c1 c2 in 
  est_libre_seg_aux c1 config vec ;;
(* 19 *)
let est_saut c1 c2 config = 
  if case_dans_config c2 config then false
  else 
    match  calcul_pivot c1 c2  with 
      None -> false 
    | Some c -> 
      if case_dans_config c config=false then false 
      else
      if  est_libre_seg c1 c config  && est_libre_seg c c2 config then true 
      else false ;;
let est_saut c1 c2 config = 
  if case_dans_config c2 config then false
  else 
    match  calcul_pivot c1 c2  with 
      None -> false 
    | Some c -> 
      if case_dans_config c config then 
        if est_libre_seg c1 c config  && est_libre_seg c c2 config then true 
        else false 
      else false ;;
(* 20 *)
(* récupérer le dernier élément de la liste  *)
let rec  last list_case = match list_case with 
    []-> ((10,10,10):case) | [c]-> ( c :case) |c::q -> last q ;;
let suiv list_case = List.hd list_case ;;(* récupérer le premier élément de la liste  *)
let rec test list_case config = 
  match list_case with 
    []-> true |[c]-> true
  |c ::q ->if est_saut c (suiv q) config then test q config else false;;
let est_saut_multiple list_case config = 
  let (case_col_list , col_list ) = config in match col_list with 
    []-> false
  |t::q -> let col =(quelle_couleur (suiv(list_case)) config)in 
    if col!=Libre && col=t then 
      if est_dans_losange (last list_case) then 
        test list_case config else false 
    else false;;
(* 21 *)
let mis_a_jour_configuration config cp = 
  match cp with 
    (Du(c1,c2))->if est_dep_unit config c1 c2=false  then Error "ce n'est pas un déplacement unitaireeeee" else
      let novelle_config = tourne_config (fait_dep_unit config c1 c2) in
      Ok novelle_config
  | (Sm([]))-> Error "Selctioner auu moins deux cases"
  |(Sm([c1]))->Error "Selctioner auuuu moins deux cases"
  |(Sm(c1::q))->if  est_saut_multiple (c1::q) config = false   then Error "ce n'est pas un déplacement multipleeeeee" else
      let novelle_config = tourne_config (fait_dep_unit config c1 (last (c1::q))) in
      Ok novelle_config;;
(* 22 *)
let rec dist list= 
  match list with 
    []-> 0 
  |[(i,j,k)]-> if (j+k < -3 )then 0 else abs(i-4)
  | (i,j,k)::q -> if (j+k < -3 )then dist q else abs(i-4) + dist q ;;
let rec  list_pion case_col_list coll new_list= match case_col_list with 
    []-> new_list
  |[((i,j,k),col)]-> if col = coll then (i,j,k)::new_list else new_list
  | ((i,j,k),col)::q-> if col = coll then list_pion q coll ((i,j,k)::new_list) 
    else list_pion q coll new_list ;;
let dist_but (config:configuration) = 
  let (case_col_list,col_list)=config in 
  let coll =List.hd col_list in 
  dist (list_pion case_col_list coll []) ;;
let gagne config = if dist_but config = 0 then true else false ;; 
(* 23 *)
let rec test_gagnant (config:configuration) = 
  let (case_col_list,col_list)=config in 
  match col_list with 
    []-> Libre 
  |t::q -> if gagne (case_col_list,[t]) then t else test_gagnant (case_col_list,q);;
let rec est_partie_aux config list_coup = 
  match list_coup with 
    []-> test_gagnant  config
  |t::q-> let nouvelle_config =  mis_a_jour_configuration config t in 
    match nouvelle_config with 
      Error x -> Dehors 
    |Ok x -> est_partie_aux x q
let est_partie config list_coup = 
  est_partie_aux config list_coup ;;
(* 24 *)
let coup_possible config case = [((case:case),((Du(case,case)):coup))];; (* on n'a pas arriver  *)
(* 25 *)
let rec list_case case_col_list joueur =(*1 pour recupérer les pions de joueur *)
  match case_col_list with 
    []->[]
  |(case,col )::q -> if col =joueur then case::list_case q joueur else list_case q joueur;;
let rec case_coup config list_case = (*2 pour recupérer tous les cas possible de déplacement *)
  match list_case with 
    []->[]
  |t::q -> let list_tmp =  coup_possible config t in 
    if list_tmp != [] then list_tmp :: case_coup config q else case_coup config q;;
let rec coup_final list_case_coup ((case_r:case),(coup_r:coup)) = (*4 pour recupérer la distance le plus petit *)
  match list_case_coup with 
    []-> (case_r,coup_r)
  |(case,coup)::q -> if dist [case] < dist [case_r] then coup_final q (case,coup) else coup_final q (case_r,coup_r);;
let rec meilleurs_coup_aux list_de_list_coup ((case:case),(coup:coup)) = (*3 pour envoyer le coup le plus proche *)
  match list_de_list_coup with 
    []-> coup
  | t::q -> meilleurs_coup_aux q (coup_final t ((case:case),(coup:coup)));;
let meilleurs_coup config =
  let (case_col_list,col_list) = config in 
  let joueur = List.hd col_list in 
  let list_de_case = list_case case_col_list joueur in 
  let case_de_coup = case_coup config list_de_case in 
  let first = List.hd (List.hd case_de_coup) in 
  meilleurs_coup_aux case_de_coup first ;;
