(***********************************************************************)
(*                                                                     *)
(*                     Astar (A* algorithm)                            *)
(*                                                                     *)
(*         David Gianazza, Ecole Nationale de l'Aviation Civile        *)
(*                                                                     *)
(*  Copyright 2017 Ecole Nationale de l'Aviation Civile.               *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Library General Public License.                            *)
(*                                                                     *)
(***********************************************************************)

(*------------------*)
(* File de priorité *)

(*------------------------------------------------*)
(* Implantation efficace, avec un maximier (heap) *)

type ('a,'b) t =
    Node of ('a,'b) node
  | Nil

and ('a,'b) node = {
  key: 'a;
  data: 'b;
  left: ('a,'b) t;
  right: ('a,'b) t}
;;

exception Empty;;

let empty = Nil;;
let is_empty t = t=Nil;;

let make k d l r = Node {key= k; data= d; left= l; right= r};;

let rec insert ?(cmp = compare) x v t =
  match t with
    Nil -> Node {key=x; data=v; left=Nil; right=Nil}
  | Node {key= y; data=d; left=l; right=r} ->
    let c= cmp x y in
    if c<=0 then (* Insert (y,d) in right branch and swap right and left. *)
      make x v (insert ~cmp y d r) l
    else (* Insert (x,v) in right branch and swap right and left. *)
      make y d (insert ~cmp x v r) l
;;

let root t =
  match t with
    Nil -> raise Empty
  | Node {key= k; data=d; left=l; right=r} -> (k,d)

let rec remove ?(cmp = compare) t =
  match t with
    Nil -> raise Empty
  | Node {left=Nil; right=r} -> r
  | Node {left=l; right=Nil} -> l
  | Node {left=l; right=r} ->
    let (x,v)= root l and (y,w)= root r in
    if cmp x y <0 then  make x v (remove ~cmp l) r
    else make y w l (remove ~cmp r)

let extract ?(cmp = compare) t =
  let (prio,x)= root t in
  (prio, x, remove ~cmp t);;

let rec elements ?(cmp = compare) q =
  if is_empty q then []
  else
    let (_,x,new_q)= extract ~cmp q in
    x:: elements ~cmp new_q;;

let rec cardinal = function
    Nil -> 0
  | Node {left= l; right= r} -> cardinal l + 1 + cardinal r;;

let rec iter f = function
    Nil -> ()
  | Node {key=k; data=d; left=l; right=r} ->
    iter f l; f k d; iter f r;;

(***
   (*----------------------------------------------------------------*)
   (* Implantation efficace, avec des arbres binaires équilibrés AVL *)

   type ('a,'b) t =
    Node of ('a,'b) node
   | Nil

   and ('a,'b) node = {
    key: 'a;
    data: 'b list;
    left: ('a,'b) t;
    right: ('a,'b) t;
    height: int };;

   let empty = Nil;;

   let is_empty t = t=Nil;;

   let height t =
   match t with
    Nil -> 0
   | Node n -> n.height

   let diff t =
   match t with
    Nil -> 0
   | Node {left=l;right=r} -> height r - height l;;

   let make k d l r =
   Node {key= k; data= d; left= l; right= r ;
        height= 1+ max (height l) (height r)};;

   let right_rotation t =
   match t with
   | Node {key=k;data=d;left=(Node {key=k1;data=d1;left=l1;right=r1});right=r} ->
      let new_r= make k d r1 r in 
      make k1 d1 l1 new_r
   | _-> failwith "right_rotation";;

   let left_rotation t =
   match t with
   | Node {key=k;data=d;left=l;right=(Node {key=k2;data=d2;left=l2;right=r2})} ->
      let new_l= make k d l l2 in
      make k2 d2 new_l r2
   | _-> failwith "left_rotation";;

   let balance t =
   match t with
   | Node ({left=l;right=r} as n) ->
      let hr= height r and hl= height l in
      let delta= hr-hl in
      if delta>1 then
   match (diff r) with
   -1 ->
   let new_r= right_rotation r in
   left_rotation (make n.key n.data l new_r)
   | 0|1 -> left_rotation t
   | _-> failwith "balance"
      else if delta<(-1) then
   match (diff l) with
   1 ->
   let new_l= left_rotation l in
   right_rotation (make n.key n.data new_l r)
   | -1|0 -> right_rotation t
   | _-> failwith "balance"
      else t
   | _-> t;;

   let rec insert ?(cmp = compare) x v t =
   match t with
    Nil -> Node {key=x; data=[v]; left=Nil; right=Nil; height=1}
   | Node ({key= y; data=d; left=l; right=r; height=h} as n) ->
      let c= cmp x y in
      if c<0 then (* Insert left *) 
        let new_l= insert ~cmp x v l in
        balance (make y d new_l r)
      else if c=0 then (* Add new value to data *)
        Node {n with key= x; data= v::d }
      else (* Insert right *)
        let new_r= insert ~cmp x v r in
        balance (make y d l new_r)
   ;;

   let rec add ?(cmp = compare) x d t =
   match t with
    Nil -> Node {key=x; data=d; left=Nil; right=Nil; height=1}
   | Node ({key= y; data=w; left=l; right=r; height=h} as n) ->
      let c= cmp x y in
      if c<0 then (* Insert left *) 
        let new_l= add ~cmp x d l in
        balance (make y w new_l r)
      else if c=0 then (* Replace old data with new data *)
        Node {n with key=x; data= d}
      else (* Insert right *)
        let new_r= add ~cmp x d r in
        balance (make y w l new_r)
   ;;

   let rec min_binding t =
   match t with
   | Node {key=k;data=d;left=Nil} -> (k,d)
   | Node {left=l} -> min_binding l
   | Nil -> failwith "min_binding"  (*raise Not_found*)
   ;;

   let rec max_binding t =
   match t with
   | Node {key=k; data=d;right=Nil} -> (k,d)
   | Node {right=r} -> max_binding r
   | Nil -> failwith "max_binding" (*raise Not_found*)
   ;;

   let rec find ?(cmp = compare) x t =
   match t with
    Nil -> raise Not_found
   | Node {key=y;data=d; left=l; right=r; height=h} ->
      let c= cmp x y in
      if c=0 then d
      else if c<0 then find ~cmp x l 
      else find ~cmp x r
   ;;

   let rec remove ?(cmp = compare) x t =
   match t with
    Nil -> failwith "remove" (*raise Not_found*)
   | Node {key=y; data=d; left=l; right=r; height=h} ->
      let c= cmp x y in
      if c=0 then (
        match (l,r) with
          Nil, Nil -> Nil
        | Nil, _-> r
        | _,Nil -> l
        | _ ->
            let (m,dm)= min_binding r in
            let new_r= remove ~cmp m r in
            balance (make m dm l new_r) )
      else if c<0 then
        let new_l= remove ~cmp x l in
        balance (make y d new_l r)
      else
        let new_r= remove ~cmp x r in
        balance (make y d l new_r)
   ;;

   exception Empty;;

   let extract _ t =
   try
    let (k,d)= min_binding t in
    match d with
      [v] -> (k,v,remove k t)
    | v::ls -> (k,v,add k ls t) (* Replace [d] with [ls] in [t] *)
    | [] -> failwith "extract"
   with Not_found -> raise Empty;;        

   let rec iter f = function
    Nil -> ()
   | Node {key=k; data=d; left=l; right=r; height=h} ->
      iter f l; List.iter (fun v -> f k v) d; iter f r;;

   let rec cardinal = function
    Nil -> 0
   | Node {left= l; right= r} -> cardinal l + 1 + cardinal r;;

   let rec right_to_left t acc =
   match t with
   | Nil -> acc
   | Node {data=d; left=l; right=r} ->
      let new_acc= right_to_left r acc in
      right_to_left l (List.rev_append d new_acc) ;;

   let elements _ t = right_to_left t [];;
 ***)

(****
   (*-------------------------------------------------------------------------*)
   (* Implantation efficace, en utilisant le module map *)
   (* Avec cette implantation, la priorité est un flottant *)
   (* A recoder avec un arbre si on veut un type polymorphique comme priorité *)

   type prio= float

   module MyMap = Map.Make (struct type t = prio let compare = compare end)

   type 'a t = 'a list MyMap.t 

   let empty= MyMap.empty

   let is_empty = MyMap.is_empty

   let size q= MyMap.fold (fun k v acc -> acc + List.length v) q 0

   let insert prio x q =
   try
    let l= MyMap.find prio q in
    MyMap.add prio (x::l) q
   with Not_found -> MyMap.add prio [x] q

   exception Empty;;

   let extract q = 
   try
    let (k,l)= MyMap.min_binding q in
    match l with
      [v] -> (k,v,MyMap.remove k q)
    | v::ls ->
        let new_map= MyMap.add k ls (MyMap.remove k q) in
        (k,v,new_map)
    | [] -> failwith "Pqueue.extract"
   with Not_found -> raise Empty

   let mem x q = MyMap.exists (fun _key l -> List.exists (fun v-> v=x) l) q

   let iter f q = MyMap.iter (fun k l -> List.iter (fun v -> f (k,v)) l) q

   let elements q =
   let ll= MyMap.fold (fun k l acc -> l::acc) q [] in
   List.flatten ll

 ***)

(*-------------------------------------------------------------------------*)
(* Implantation inefficace, avec des listes *)
(* Pour une implantation efficace, utiliser des arbres binaires équilibrés *)

(***
   type ('a,'b) t= ('a * 'b) list;;

   let empty = [];;      

   let is_empty q = q=[]

   let size= List.length;;

   let insert ?(cmp=compare) key x pqueue =
   let rec loop l =
    match l with
      (ky,y)::ls ->
   if cmp key ky < 0 then (key,x)::l
   else (ky,y)::loop ls
    | [] -> [key,x] in
   try loop pqueue
   with _-> failwith "insert";;


   exception Empty;;

   let extract pqueue =
   match pqueue with
    (prio,x)::xs-> (prio,x,xs)
   | _-> raise Empty;;

   let rec mem x q = 
   match q with
    (_prio,y)::_ when x=y -> true
   | _::ls -> mem x ls
   | [] -> false

   let iter f (q: ('a, 'b) t)= List.iter (fun (prio,x) -> f prio x) q

   let elements q = List.map (fun (prio,u) -> u) q
 ***)
