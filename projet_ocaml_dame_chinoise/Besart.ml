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
(* est_saut_multiple [(3, -3, 0); (-1, -3, 4); (-3, -1, 4); (-3, 3, 0); (-1, 3, -2); (1, 1, -2)]
   ([(3, -3, 0),Vert;(1, -3, 2),Vert; (-2, -2, 4),Bleu; (-3, 1, 2),Rouge; (-2, 3, -1),Vert; (0, 2, -2),Jaune] 
   ,([Vert;Bleu;Jaune;Rouge]));; *)
(* 21 *)
est_saut_multiple [(-4, 1, 3); (-3, 1, 2)]([(-4, 3, 1),Vert] 
                                          ,([Vert;Bleu;Jaune;Rouge])) ;;
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
mis_a_jour_configuration ([(-3, 2, 1),Rouge;],([Vert;Bleu;Jaune;Rouge])) (Sm[(-4, 2, 2); (-2, 2, 0)]);;


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

(* dist_but ([(5, -3, -2),Vert; (5, -2, -3),Vert]
         ,([Vert;Bleu;Jaune;Rouge]));; *)
let gagne config = if dist_but config = 0 then true else false ;; 