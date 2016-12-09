open Game


let rec best_move state =
  match result state with
	|Some x -> (None,x)
	|_ -> 
		let liste =all_moves state in
		let rec loop liste best_mov best_r =
			match liste with
				|[] -> (best_mov,best_r)
				|x::suite -> 
					let playeur = turn state in
					let (_,d) = best_move (play state x) in
						 match (compare playeur (best_r) (d)) with
							|Smaller -> loop suite best_mov best_r
							|Equal -> loop suite x best_r
							|_ -> (x,d)
						
		in
		let (x,y) = loop liste (List.hd liste) (worst_for (turn state)) in
		(Some x,y)
;;

