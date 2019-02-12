
(* fonction qui renvoie la valeur d'une clique, il prend un Set en parametre et renvoie la somme des poids de ses elements *)
let func_eval = fun c ->
  Graph.SS.fold (fun a b -> a.Graph.weight + b) c 0
 
(* fonction de comparaison utilisee dans la Pqueue *) 
let cmp = fun x y-> 0-(compare x y)

(* cette fonction renvoie true si l'ensembe PA n'est pas vide ou si OM n'est pas vide et ameliore la recherche,
et renvoie false sinon *) 
let in_local_search = fun graph nbIter tl pa om c ->
  let pa_cond = not (Pqueue.is_empty pa) in
  let om_cond = (if not (Pqueue.is_empty om) then let (prio, _, _) = Pqueue.extract ~cmp om in prio > 0 else false) in
  pa_cond || om_cond

(* fonction qui renvoie true se le set passe en parametre constitue une clique valide*)
let isClique = fun graph clique ->
  (* on teste chaque élément de la clique *)
  let checkElt = fun u ->
    let checkOthers = fun v ->
      if (u=v) then true
      else Graph.is_voisin graph u v
    in  
    Graph.SS.for_all checkOthers clique
  in
  Graph.SS.for_all checkElt clique
    
(*fonction principale de la recherche*)
let bls = fun graph t ->
  (*Graphics.open_graph (Printf.sprintf " %dx%d+50-0" 800 800);*)
  Random.self_init () ;
  (* on commence par initialiser les ensembles, les variables et parametres utilises *)
  let c = ref (Move.first_solution graph) in (* la clique courante *)
  let pa = ref Pqueue.empty in
  let om = ref (Move.create_om graph !c) in
  let fc = ref (func_eval !c) in 	(* la valeur de la fonction objective*)
  (*let fc_array = ref [|(0, !fc * 5)|] in*)
  let cbest = ref !c in  	(* la meilleure clique *)
  let fbest = ref !fc in  	(* la meilleur valeure de la fonction objective *)
  let cp = ref !c in  		(* le dernier optimum local *)
  let w = ref 0 in  		(* compteur pour les optima locals consecutifs sans amelioration *)
  let l0 = (Array.length graph.Graph.nodes) / 100 in
  let lmax = (Array.length graph.Graph.nodes) / 10 in
  let l = ref l0 in
  let tl = Array.map (fun a -> (0-max_int, 0)) graph.Graph.nodes in (* inialisation de la tabou list *)
  let nbIter = ref 0 in
  
  let alpha_r = 0.8 and alpha_s=0.8 and p0 = 0.75 in (*alpha_r = 0.83 and alpha_s=0.58 and p0 = 0.63 in*)
  
  (* boucle principale de la recherche *)
  while !nbIter < 10000 do
    while in_local_search graph !nbIter tl !pa !om !c do (* tant que PA est non vide ou OM est non vide et ameliore la recherche*)
      if Pqueue.is_empty !pa then (* on applique M2 si M1 n'est pas applicable *)
	(let m = Move.M2 in
	Move.apply_move m graph c pa om fc !nbIter tl)
      else if Pqueue.is_empty !om then (* on applique M1 si M2 n'est pas applicable *)
	(let m = Move.M1 in
	Move.apply_move m graph c pa om fc !nbIter tl)
      else
	(let m = Move.best_move !pa !om !fc in    
	Move.apply_move m graph c pa om fc !nbIter tl);   (* on applique le meilleur mouvement entre M1 et
	la mise a jour des ensembles PA et OM et la tabou liste se fait dans la fonction apply_move*)
      nbIter := !nbIter + 1
    done;
    (*fc_array:= Array.append !fc_array [|(!nbIter, !fc*5)|];
    Draw.draw graph !c;
    Graphics.draw_poly_line !fc_array;*)
    begin
      if fc > fbest then	(* si vrai, on met a jour la meilleure solution, et on remet le compteur a 0 *)
        (cbest := !c;
         fbest := !fc;
         w := 0) 
      else
        w := !w + 1
    end;
    begin 
      if float_of_int !w > t then (* si le compteur est superieur au parametre t 
      c.a.d on a pas une meilleure solution pour t iteration, on met l la puissance de la perturbation a lmax *) 
        (l := lmax;
         w := 0)
      else if !c = !cp then
        l := !l + 1
      else (* on a trouve une meilleure solution on met la puissance de la perturbation au niveau l0 *)
        l := l0
    end;
    
    cp := !c;
    (* on applique la fonction perturbation *)
    Perturbation.perturbation graph pa om fc c !l tl nbIter !w alpha_r alpha_s t p0 fbest;
  done;
  (!fbest, !cbest);;

let ()=
  if (Array.length Sys.argv) = 2 then
	begin
	  let demo = Graph.create_graph_DIMACS Sys.argv.(1) in
	  let time_start = Unix.time () in
	  let (fbest, c) = bls demo 1000. in
	  let time_end = Unix.time () in
	  Printf.printf "durrée : %f\n" (time_end -. time_start);
	  Printf.printf "fbest: %d\n" fbest;
  	  if isClique demo c then Printf.printf "OK\n"
	end
  else Printf.printf "Entrez le path du fichier DIMACS\n" ;;
