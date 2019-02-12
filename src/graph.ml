
(* type node qui represente les noeuds du graphe *)
type node = {
	id : int ;
	weight : int ;
}

(* on initialise le module de la structure Set, qui sera un Set de node *) 
module SS = Set.Make(
	struct
    let compare = Pervasives.compare
    type t = node
  end		
)

(* type graph qu'on va utiliser pour representer les graphes *)
type graph = {
	nodes : node array ;
	edges :  SS.t array 
}


(* pour chaque fonction il y a une autre dont le nom se termine par _id, la seule difference et que les _id prend
les ids entiers des noeuds en parametre au lieu des nodes *)

(* une liste des voisins de u , pour avoir les poids parcourir la liste 'nodes' *)
let get_voisins = fun graph u ->
	SS.elements graph.edges.(u.id - 1)

let get_voisins_id = fun graph u_id ->
	SS.elements graph.edges.(u_id - 1)

let get_node_id = fun graph u_id ->
	graph.nodes.(u_id - 1 ) 

(* creation d'un graphe de n sommets de poids 1 *)
let create_graph = fun n ->
	{nodes = Array.init n (fun i -> {id = i + 1; weight = 1}) ; edges = Array.make n SS.empty}


(* change le poids de u en w *)
let change_weight = fun graph u w ->
	Array.set graph.nodes (u.id - 1) {id = u.id; weight = w}

let change_weight_id = fun graph u_id w ->
	Array.set graph.nodes (u_id - 1) {id = u_id; weight = w}

(* renvoie le poids d'un noeud *)
let get_weight = fun u ->
	u.weight

let get_weight_id = fun graph u_id ->
	graph.nodes.(u_id - 1).weight

(* ajoute un arete entre u et v *)
let add_edge = fun graph u v ->
	Array.set graph.edges (u.id - 1) (SS.add v (graph.edges.(u.id - 1) ) ) ;
	Array.set graph.edges (v.id - 1) (SS.add u (graph.edges.(v.id - 1) ) )

let add_edge_id = fun graph u_id v_id ->
	Array.set graph.edges (u_id - 1) (SS.add (graph.nodes.(v_id - 1) ) (graph.edges.(u_id - 1) ) );
	Array.set graph.edges (v_id - 1) (SS.add (graph.nodes.(u_id - 1) ) (graph.edges.(v_id - 1) ) )


(* true si u est voisin de v, false sinon *)
let is_voisin = fun graph u v ->
	SS.exists (fun x -> u = x) (graph.edges.(v.id - 1))
	
let is_voisin_id = fun graph u_id v_id ->
	SS.exists (fun x -> u_id = x.id) (graph.edges.(v_id - 1))
	

(* retourne un graphe avec un sommet d'un poids w ajoute *)
let add_node = fun graph w ->
	{nodes = Array.append graph.nodes [|{id = Array.length graph.nodes ; weight = w}|] ;
    	edges = Array.append graph.edges [|SS.empty|] }


(* fonction qui renvoie un graphe demo fixe*)
let graph_demo = fun () -> 
	let g = create_graph 5 in
	add_edge_id g 1 2 ;
	add_edge_id g 1 4 ;
	add_edge_id g 1 5 ;
	add_edge_id g 2 3 ;
	add_edge_id g 2 4 ;
	add_edge_id g 2 5 ;
	add_edge_id g 3 4 ;
	add_edge_id g 4 5 ;
	g




(* fonction qui genere des liaisons entre les noeuds d'un graphe donne avec une densite donnee 
pour chaque paire de noeud il sont voisins avec une probabilite specifiee en parametres *)

let generate_voisins_all = fun graph densite ->
	let n = (Array.length graph.nodes) in
	let rec gen = fun graph i ->
		if i = n+1 then graph
		else (
			let rec gen2 = fun j ->
				if j = n+1 then ()
				else (let r = Random.float 1. in
					if densite > r then add_edge_id graph i j; gen2 (j+1) )
			in (gen2 (i+1) ); gen graph (i+1) )
	in ( gen graph 1 )
		

(* generer un graphe aleatoire non pondere de n sommets et de densite  0 <= d <= 1 *)

let generate_random_graph_unweighted = fun n densite ->
	generate_voisins_all (create_graph n) densite
	
	
(* generer un graphe aleatoire pondere de n sommets et un poids maximal de max_w *)

let generate_random_graph = fun n densite max_w ->
	let g_bis = create_graph n in
	let rec gen_w = fun i ->
		if i = (n + 1) then g_bis
		else let r = Random.int max_w in
		change_weight_id g_bis i r ; gen_w (i+1)
	in generate_voisins_all (gen_w 1) densite 
		



(* Conversion fichier DIMACS vers graph, la fonction renvoie un resultat de type graph construit en se basant sur le fichier DIMACS *)

let create_graph_DIMACS = fun filename ->
	let ic = open_in filename in
	let comment_flag = ref true in
	let num_edges = ref 0 in
	let num_nodes = ref 0 in
		while !comment_flag do
			let line = input_line ic in
			let line_type = String.get line 0 in
			if line_type = 'p' then 
				(let l = Str.split (Str.regexp " ") line in
				num_edges := int_of_string (List.nth l 3);
				num_nodes := int_of_string (List.nth l 2);
				comment_flag := false )
		done;
		let graph = create_graph !num_nodes in
		for i = 1 to !num_edges do
			let line = input_line ic in
			let l = Str.split (Str.regexp " ") line in
			add_edge_id graph (int_of_string (List.nth l 1) ) (int_of_string (List.nth l 2))
		done;
		graph
	

(* Conversion fichier DIMACSW vers graph, la fonction renvoie un resultat de type graph construit en se basant sur le fichier DIMACSW*)
let create_graph_DIMACSW = fun filename ->
	let ic = open_in filename in
	let comment_flag = ref true in
	let num_edges = ref 0 in
	let num_nodes = ref 0 in
		while !comment_flag do
			let line = input_line ic in
			let line_type = String.get line 0 in
			if line_type = 'p' then 
				(let l = Str.split (Str.regexp " ") line in
				num_edges := int_of_string (List.nth l 3);
				num_nodes := int_of_string (List.nth l 2);
				comment_flag := false )
		done;
		let graph = create_graph !num_nodes in
		for i=1 to !num_nodes do
			change_weight_id graph i ((i mod 200) + 1)
		done;
		for i = 1 to !num_edges do
			let line = input_line ic in
			let l = Str.split (Str.regexp " ") line in
			add_edge_id graph (int_of_string (List.nth l 1) ) (int_of_string (List.nth l 2))
		done;
		graph







