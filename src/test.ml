let () =
	let n = 100 in
  for j = 0 to 10 do
    let filename = String.concat "" ["stats";(string_of_int j);".txt"] in
    let dimacs_file = String.concat "" ["data/dimacs_test";(string_of_int j);".txt"] in
    let fic = open_out filename in
    let demo = Graph.create_graph_DIMACS dimacs_file in
    let avg = ref 0. in
    let max_val = ref 0 in
    let min_val = ref max_int in
    let sigma = ref 0. in
    let fbest_list = ref [] in
    
    for i = 0 to (n-1) do
		try
		  (let (fbest, c) = Main.bls demo 1000. in
		  avg := !avg +. (float_of_int fbest);
		  fbest_list := fbest::(!fbest_list);
		  min_val := min fbest !min_val;
		  max_val := max fbest !max_val;
		  Printf.fprintf fic "%d %d\n" i fbest;
		  if Main.isClique demo c then Printf.fprintf fic "OK\n" else Printf.fprintf fic "NOT OK\n")
		with
		_ -> ()
    done;
	let m = List.length !fbest_list in
    avg := !avg /. (float_of_int m);
    Printf.fprintf fic "\navg = %f" !avg;
    List.iter (fun elt -> sigma := !sigma +. ((float_of_int elt) -. !avg)**2.) !fbest_list;
    sigma := sqrt (!sigma /. (float_of_int m));
    Printf.fprintf fic ",    sigma = %f" !sigma;
    Printf.fprintf fic ",    min = %d" !min_val;
    Printf.fprintf fic ",    max = %d" !max_val;
    close_out fic
  done;;
