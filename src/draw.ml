let width= 800
let height= 800
let m = 200.
let red = Graphics.rgb 255 0 0 and blue = Graphics.rgb 0 0 255

let pi = 4.0 *. atan 1.0

let calcul_points = fun m graph c ->
  let n = Array.length graph.Graph.nodes in
  let f = fun i a ->
    let float_i = float_of_int i and float_n = float_of_int n in
    let alpha = (2.)*.float_i*.pi/.float_n in
    if Graph.SS.mem a c then (int_of_float (m*.cos alpha) + 400, int_of_float (m*.sin alpha) + 500, red, a) 
    else (int_of_float (m*.cos alpha) + 400, int_of_float (m*.sin alpha) + 500, blue, a) in
  Array.mapi f graph.Graph.nodes

let draw_points = fun points ->
  Array.iter (fun (x, y, color, a) ->
    begin
      Graphics.set_color color;
      Graphics.draw_circle x y 2;
      Graphics.fill_circle x y 2
    end) points
    
let draw_edge = fun graph points ->
  let f = fun (x, y, color, a) ->
    Graphics.moveto x y;
    List.iter (fun node -> 
      let (x2,y2,col,_) = points.(node.Graph.id - 1) in
      if color = col then
        if col = red then
	  begin
	    Graphics.set_color color;
            Graphics.set_line_width 2;
	    Graphics.lineto x2 y2;
	    Graphics.moveto x y;
            Graphics.set_line_width 1;
	  end
        else
			begin
			();
	    (*Graphics.set_color color;
	    Graphics.lineto x2 y2;
	    Graphics.moveto x y;*)
	  end
      else 
	begin
		();
	  (*Graphics.set_color blue;
	  Graphics.lineto x2 y2;
	  Graphics.moveto x y;*)
	end) (Graph.get_voisins graph a) in
  Array.iter f points
	
let draw = fun graph c ->
	Graphics.auto_synchronize false;
	Graphics.clear_graph ();
  let points = calcul_points m graph c in
  draw_points points;
  draw_edge graph points;
	Graphics.auto_synchronize true;;
