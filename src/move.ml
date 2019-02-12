(* type mouvement*)
type t = M1 | M2 | M3 of Graph.node  | M4 of float

(* fonction de comparaison utilisee dans la Pqueue *) 
let cmp = fun x y -> 0-(compare x y)
    
(* authorized graph u l renvoie la liste des noeuds de la liste l qui sont voisins de u *)
let authorized = fun graph u l->
  List.filter (fun a-> Graph.is_voisin graph a u) l
    
(* first_solution graph renvoie une clique maxiximale (dans le sens on ne peut plus ajouter de noeud à la clique) dont le noeud initial est choisi au hasard *)
let first_solution = fun graph ->
  let u = Graph.get_node_id graph (Random.int (1 + Array.length graph.Graph.nodes)) in
  let rec update = fun c auth_list ->	(* mettre à jour la liste des noeuds autorisés à être ajouter à la clique *)
    match auth_list with
      [] ->  c							(* on arrête quand on ne peut plus ajouter de noeud à la clique *)
    |tete::queue -> update (Graph.SS.add tete c) (authorized graph tete queue) in
  update (Graph.SS.singleton u) (Graph.get_voisins graph u)
    
(* list_to_pqueue l renvoie une pqueue composée des noeuds de la liste l ordonnés selon leurs poids *)
let list_to_pqueue = fun l ->
  let q = Pqueue.empty in
  List.fold_left (fun a b -> Pqueue.insert ~cmp b.Graph.weight b a) q l
    
(* create_pa_liste graph c renvoie l'ensemble PA qui est une pqueue composé des noeuds autorisés à être ajouté à la clique c*)
let create_pa1 = fun graph c->
  let rec create_pa_list = fun l current_pa->
    match l with
      [] -> current_pa
    | tete::queue -> create_pa_list queue (authorized graph tete current_pa) in
  let pa_list = create_pa_list (Graph.SS.elements c) (Array.to_list graph.Graph.nodes) in
  list_to_pqueue pa_list

(* create_pa2 est une autre version de la fonction create_pa1, elle le meme parametre et renvoie le meme resultat*)
let create_pa2 = fun graph c->
  let n = Graph.SS.cardinal c in
  let c_array = Array.of_list (Graph.SS.elements c) in
  let rec iter_list = fun l pa->
    match l with
      [] -> pa
    | v::queue ->
	begin
	  let exist = Graph.SS.exists (fun x -> x=v) c in
	  let vid = v.Graph.id in
	  let cnt = ref 0 in
          let i = ref 0 in
	  if not exist then
	    (
	     while (if !i < n then Graph.is_voisin_id graph vid (c_array.(!i).Graph.id) else false) do 
	       cnt := !cnt + 1;
               i := !i + 1
	     done;
	     if !cnt = n then iter_list queue (Pqueue.insert ~cmp (Graph.get_weight_id graph vid) v pa)
	     else iter_list queue pa
	    )
	  else iter_list queue pa
	end in
  iter_list (Array.to_list graph.Graph.nodes) (Pqueue.empty)
    

     
(* create_om graph c revoie la pqueue de tous les couple de noeuds (u, v) pouvant être "swaper" ordonnés selon (v.weight - u.weight) *) 
let create_om = fun graph c ->
  let n = Graph.SS.cardinal c in
  let c_array = Array.of_list (Graph.SS.elements c) in
  let rec iter_list = fun l om->
    match l with
      [] -> om
    | v::queue ->
	begin
	  let exist = Graph.SS.exists (fun x -> x=v) c in
	  let uid = ref 0 in
	  let vid = v.Graph.id in
	  let cnt = ref n in
          let i = ref 0 in
	  if not exist then
	    (
	     while (!i < n && !cnt >= n-1) do
	       if not (Graph.is_voisin_id graph vid (c_array.(!i).Graph.id)) then
                 (cnt := !cnt - 1;
                  uid := c_array.(!i).Graph.id)
               else ();
               i := !i+1
	     done;
	     if !cnt = n - 1 then ( iter_list queue (Pqueue.insert ~cmp ((Graph.get_weight_id graph vid) - (Graph.get_weight_id graph !uid)) (graph.Graph.nodes.(!uid-1),v ) om))		
	     else iter_list queue om
	    )
	  else iter_list queue om
	end in
  iter_list (Array.to_list graph.Graph.nodes) (Pqueue.empty)



let new_pa_om = fun m graph c pa om ->
  match m with
    M1 ->
      begin
        let (prio, v, reste_pa) = Pqueue.extract ~cmp pa in
        let new_pa = ref Pqueue.empty and new_om = ref Pqueue.empty in
        Pqueue.iter (fun prio elt ->
          if Graph.is_voisin graph v elt then
            new_pa := Pqueue.insert prio elt !new_pa
          else
            new_om := Pqueue.insert ~cmp (Graph.get_weight elt - prio) (v, elt) !new_om) reste_pa;
        Pqueue.iter (fun prio (a, elt) ->
          if Graph.is_voisin graph v elt then
            new_om := Pqueue.insert ~cmp prio (a, elt) !new_om) om;
        (!new_pa, !new_om);
      end
  | M2 ->
      begin
        let (prio, (u ,v), reste_om) = Pqueue.extract ~cmp om in
        let new_pa = ref Pqueue.empty in
        Pqueue.iter (fun prio elt ->
          if Graph.is_voisin graph v elt then
            new_pa := Pqueue.insert ~cmp prio elt !new_pa) pa;
        Pqueue.iter (fun prio (a, elt) ->
          if a = u && Graph.is_voisin graph v elt then
            new_pa := Pqueue.insert ~cmp (Graph.get_weight elt) elt !new_pa) reste_om;
        (!new_pa, create_om graph c);
      end
  | M3 u ->
      begin
        let new_pa = ref pa in
        new_pa := Pqueue.insert ~cmp (Graph.get_weight u) u !new_pa;
        Pqueue.iter (fun prio (a, elt) -> if a = u then new_pa := Pqueue.insert (Graph.get_weight elt) elt !new_pa) om;
        (!new_pa, create_om graph c);
      end
  | M4 alpha -> (create_pa2 graph c, create_om graph c)
    
 (* evalue un mouvement et retourne la nouvelle valeur de la fonction objective *)   
let eval_move = fun m pa om obj->
  match m with
    M1 ->
      begin
        let (prio,_,_) = Pqueue.extract ~cmp pa in
        obj + prio
      end
  | M2 ->
      begin
        let (prio,_,_) = Pqueue.extract ~cmp om in
        obj + prio
      end
  | M3 node -> 
      obj - (Graph.get_weight node) 
  | _ -> failwith "non utilisée"

(* renvoie le meilleur mouvement entre M1 et M2 *)
let best_move = fun pa om obj ->
  let obj_m1 = eval_move M1 pa om obj in
  let obj_m2 = eval_move M2 pa om obj in
  if (obj_m1 < obj_m2) then M2 else M1

(* fonction qui applique le mouvement passe en parametre, il fait la mise a jour de la clique des ensembles PA et OM, et de 
la tabou list *)
let apply_move = fun m graph c pa om obj iter tl ->
  match m with
    M1 ->  
      begin
        let (prio, v, reste_pa) = Pqueue.extract ~cmp !pa in
        c := Graph.SS.add v !c; (* on ajoute le noeud avec le poids maximal dans la clique*)
        obj := !obj + prio;
        let (new_pa, new_om) = new_pa_om m graph !c !pa !om in
	pa := new_pa;
	om := new_om;
      end
  | M2 ->
      begin
        let (prio, (u, v), reste_om) = Pqueue.extract ~cmp !om in 
	let i = u.Graph.id - 1 in
        let phi = 7 in 				
	let gamma =  phi + (Random.int (1 + (Pqueue.cardinal !om))) in  
	Array.set tl i (iter,gamma); (* on met a jour la tabou liste *)
        c := Graph.SS.remove u !c;   (* on fait un swap entre u et v *)
        c := Graph.SS.add v !c;
        obj := !obj + prio;
        let (new_pa, new_om) = new_pa_om m graph !c !pa !om in
	pa := new_pa;
	om := new_om;
      end
  | M3 node ->
      begin
	let i = node.Graph.id - 1 in
	let phi = 7 in 				
	let gamma =  phi + (Random.int (1 + (Pqueue.cardinal !om))) in  
	Array.set tl i (iter,gamma);
	c := Graph.SS.remove node !c; (* on retire un noeud aleatoire de la clique pour perturber *)
	obj := !obj - node.Graph.weight;
        let (new_pa, new_om) = new_pa_om m graph !c !pa !om in
	pa := new_pa;
	om := new_om;
      end;
  | M4 alpha ->
      begin
        let list_nodes = Array.to_list graph.Graph.nodes in
        let rec iter_list = fun l ->
            match l with
              [] -> ()
            | v::queue -> (* on parcours l'ensemble OC, qui est l'ensemble des noeuds qui n'appartient pas a la clique pour trouver
	    		     un noeud qui verifie la condition  ci-dessous *)
                begin
                  let exist = Graph.SS.exists (fun x -> x=v) !c in
                  let neighbour = ref Graph.SS.empty in
				  let new_tl = Array.map (fun elt -> elt) tl in
                  let somme = (Graph.SS.fold
                                 (fun x somme -> if (Graph.is_voisin graph x v) then
                                   (
                                    neighbour := Graph.SS.add x !neighbour;
                                    x.Graph.weight + somme
                                   )
                                 else
				   let i = x.Graph.id - 1 in
				   let phi = 7 in 				
				   let gamma =  phi + (Random.int (1 + (Pqueue.cardinal !om))) in  
				   Array.set new_tl i (iter,gamma);
                                   somme
                                 ) !c v.Graph.weight) in
                  let condition = (float_of_int somme) > (alpha *. (float_of_int !obj)) in 
                  if ((not exist) && condition) then
                    begin
		      obj := somme;
		      c := Graph.SS.add v !neighbour;
                      let (new_pa, new_om) = new_pa_om m graph !c !pa !om in
	              pa := new_pa;
	              om := new_om;
		      if 0.5 > Random.float 1. then iter_list queue else Array.iteri (fun i elt-> Array.set tl i (new_tl.(i))) tl
                    end
                  else
                    iter_list queue
                end
        in
        iter_list list_nodes
      end;
      
(*let ()=
  let demo = Graph.generate_random_graph 10 0.5 10. in
  let init = first_solution demo in
  Printf.printf "%d" (List.length (Graph.SS.elements init));
  Draw.draw demo init;;*)
      
