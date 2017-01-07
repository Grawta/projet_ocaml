open Game
open Gamebase
open Functory.Network
open Functory.Network.Same

let memory = Hashtbl.create 200000
let cache f =
  fun arg arg_useless ->
  (*Printf.printf "size de la hash : %d \n %!" (Hashtbl.length memory) ; *)
    if Hashtbl.mem memory (arg, arg_useless) then Hashtbl.find memory (arg,arg_useless)
    else
      begin
        let res = f arg arg_useless in
          Hashtbl.add memory (arg,arg_useless) res ;
          res
      end
;;



(* IA qui marche avec stack overflow
 *)
(* let rec best_move state =
  match result state with
	|Some x -> (None,x)
	|_ -> 
		let liste =all_moves state in
		let rec loop liste best_mov best_r =
			match liste with
				|[] -> (best_mov,best_r)
				|x::suite -> 
					let playeur = next (turn state) in
					let (_,d) =  best_move (play state x) in
						 match (compare playeur (best_r) (d)) with
							|Smaller -> loop suite best_mov best_r
							|Equal -> loop suite x best_r
							|_ -> (x,d)
						
		in
		let (x,y) = loop liste (List.hd liste) (worst_for (next (turn state))) in
		(Some x,y)
;;
 *)




(*Version avec cache*)
let best_move_cache state  =
  let rec best_move2 state cpt =
  match result state with
	|Some x -> (None,x)
	|_ -> 
		let liste =all_moves state in
		let rec loop liste best_mov best_r  =
			match liste with
				|[] -> (best_mov,best_r)
				|x::suite -> 
				if(cpt > 4) then
					(x,calcul state)
				else 
					let playeur =turn state in
					let (_,d) = cache best_move2 (play state x) (cpt+1) in
						 match (compare playeur (best_r) (d)) with
							|Smaller ->  loop suite best_mov best_r 
							|Equal ->  	loop suite x d 
							|_ -> loop suite x d
						
		in
		let (x,y) = loop liste (List.hd liste) (worst_for (turn state))  in
		(Some x,y)
	in
	 best_move2 state 0
;;



let comparebis player a b = 
  match compare player (snd a) (snd b) with
  | Greater -> b
  | _ -> a;;

(*Version sans cache pour faire le functory*)
let best_move_bis state  =
  let rec best_move2 state cpt =
  match result state with
	|Some x -> (None,x)
	|_ -> 
		let liste =all_moves state in
		let rec loop liste best_mov best_r  =
			match liste with
				|[] -> (best_mov,best_r)
				|x::suite -> 
				if(cpt > 4) then
					(x,calcul state)
				else 
					let playeur =turn state in
					let (_,d) =  best_move2 (play state x) (cpt+1) in
						 match (compare playeur (best_r) (d)) with
							|Smaller ->  loop suite best_mov best_r 
							|Equal ->  	loop suite x d 
							|_ -> loop suite x d
						
		in
		let (x,y) = loop liste (List.hd liste) (worst_for (turn state))  in
		(Some x,y)
	in
	 best_move2 state 0
;;

let best_move state = 
  match result state with
  | Some(x) -> (None,x)
  | None ->
    let moves = List.filter (is_valid state) (all_moves state) in
    let zemap = fun move -> Some(move, snd (best_move_bis (play state move))) in
    let zefold = fun a b -> match (a, b) with
      | None, _ -> b
      | _, None -> a
      | Some a, Some b -> Some(comparebis (turn state) a b)
    in
    match map_fold_ac ~f:zemap ~fold:zefold None moves with
      | None -> (None,calcul state)
      | Some(move, result) -> Printf.printf "Functory Done : on a une solution\n%!" ; (Some move,result)
