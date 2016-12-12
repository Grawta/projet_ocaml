open Gamebase

(* These types are abstract in game.mli *)
type state = (int matrix) * player
type piece = int 
type move = int*piece
(*On fait des moves le pad ex: 9 = diagonale droite 8=haut*)
type result = int


let carte_init =  [|[|5;6;7;8|];
                  [|0;0;0;0|];
                  [|0;0;0;0|];
                  [|1;2;3;4|]|]
;;

(* Printers *)
let state2s (n,p) = Printf.sprintf "Current = %s  // %s to play" (matrix2s n string_of_int) (player2s p)
let move2s (y,p) = Printf.sprintf " Mouvement %d par la piece numero : %d" y p
let result2s  n = match n with
	|(-5) -> (player2s Comput) ^ " wins"
	|(5) -> (player2s Human) ^ " wins"
	| _ -> failwith "Euh no winner not cool"

(* Reader *)
let readmove s=     try Some (Scanf.sscanf s "%d %d" (fun n x -> (n,x)))
  with _ -> None
;;
(* You have to provide these. *)
let initial = (carte_init,Human)
;;

let turn = function
  |(_,playeur)-> playeur
;;


(*Fonction qui recup les coordonnees grace a l'id de la piece*)

let coord (m,_) p =find_cell m (fun x -> x==p);;

let is_valid stat mov= 
match stat,mov with
  |(matrice,Human),(mouvement,piece)-> 
  if (mouvement <1 )||(mouvement>9)||(mouvement==5)then
    false
  else
   	if ((piece > 4)||(piece < 1)) then 
  		false 
  	else
  		begin match coord stat piece with
      | None -> false 
      | Some(x,y) -> let (new_x,new_y) = 
        begin match mouvement with 
          |1 -> (x+1,y-1)
          |2 -> (x+1,y)
          |3 -> (x+1,y+1)
          |4 -> (x,y-1)
          |6 -> (x,y+1)
          |7 -> (x-1,y-1)
          |8 -> (x-1,y)
          |9 -> (x-1,y+1)
          |_ -> failwith "Marche pas Humain"
        end
        in 
      if ((new_x <0)||(new_y <0)||(new_x>3)||(new_y>3)) then
        false
      else
          let p = matrice.(new_x).(new_y) in
          (p > 4)||(p==0) 
           
       end 
   |(matrice,Comput),(mouvement,piece)->
    if (mouvement <1 )||(mouvement>9)||(mouvement==5)then
    false
    else
    if ((piece < 5)||(piece > 8)) then 
  		false 
  	else
  		begin match coord stat piece with
  		| None -> false 
  		| Some(x,y) -> let (new_x,new_y) = begin match mouvement with 
  				|1 -> (x+1,y-1)
          |2 -> (x+1,y)
          |3 -> (x+1,y+1)
          |4 -> (x,y-1)
          |6 -> (x,y+1)
          |7 -> (x-1,y-1)
          |8 -> (x-1,y)
          |9 -> (x-1,y+1)
  				|_ -> failwith "Marche pas Comput"
  		end
  		in 
  		if ((new_x <0)||(new_y <0)||(new_x>3)||(new_y>3)) then
  			false
  		else
  	  		let p = matrice.(new_x).(new_y) in
  	  		 p < 5
  	  end
;;


let play (current,player) (n,p)= 
if (is_valid (current,player) (n,p)) then 
   match coord (current,player) p with
      | None -> (current,player) 
      | Some(x,y) -> 		let matrice = clone_matrix current in
	                      begin match (matrice,player)  with
    		                  | (matrice,_) ->  
                            matrice.(x).(y) <- 0 ;
                             match n with
                             |1 -> (matrice.(x+1).(y-1) <- p);(matrice, (next player))
                             |2 -> (matrice.(x+1).(y) <- p);(matrice, (next player))
                             |3 -> (matrice.(x+1).(y+1) <- p);(matrice, (next player))
                             |4 -> (matrice.(x).(y-1) <- p);(matrice, (next player))
                             |6 -> (matrice.(x).(y+1) <- p);(matrice, (next player))
                             |7 -> (matrice.(x-1).(y-1) <- p);(matrice, (next player))
                             |8 -> (matrice.(x-1).(y) <- p);(matrice, (next player))
                             |9 -> (matrice.(x-1).(y+1) <- p);(matrice, (next player))
                             |_ -> failwith "Marche pas 3"
                        end
else
  (current,player)
;;

let all_moves stat =
  let rec loop stat (n,p) = 
  if (p <9) then
    if (n<10) then
      if (is_valid stat (n,p)) then 
       (n,p)::(loop stat (n+1,p))
      else 
       loop stat ((n+1),p)
    else
      loop stat (1,p+1)
  else
    []
  in loop stat (1,1) 
;;

(*let result = function
  |(current,Human) -> if(find_cell current (fun x -> (x < 5 && x > 0)) = None) then Some (Win Comput) else None
  |(current,Comput) ->if(find_cell current (fun x -> x >4) = None) then Some (Win Human) else None
;;*)
let calcul (current,_) =
			let rec loop1 current num cpt =
						if (num <5) then
							begin match find_cell current (fun x -> (x=num)) with
                       		 |None -> loop1 current (num+1) cpt
                       		 |_-> loop1 current (num+1) (cpt +1) 
                       		end
                       	else if (num <9) then
							begin match find_cell current (fun x -> (x=num)) with
                       		 |None -> loop1 current (num+1) cpt
                       		 |_-> loop1 current (num+1) (cpt -1) 
                       		end
                       	else
                       		cpt
                       	in
                       	loop1 current 1 0
	
;;


let result = function
  |(current,Human) -> begin match find_cell current (fun x -> (x < 5 && x >0)) with
                        |None -> Some (-5)
                        |_ -> None
                      end
  |(current,Comput) ->begin match find_cell current (fun x -> x > 4) with
                        |None -> Some (5)
                        |_ -> None
                      end
;;



(* This type was given in game.mli.
 * We have to repeat it here. *)
type comparison = Equal | Greater | Smaller

(* let compare playeur r1 r2 = match (playeur,r1,r2) with
	|Human,Win Human, Win Comput |Comput , Win Comput, Win Human -> Smaller
	|Human, Win Comput, Win Human | Comput,Win Human, Win Comput -> Greater
	|_,_,_ -> Equal  
;; *)



let compare playeur r1 r2 = match playeur with
	|Human  -> if (r1 > r2) then
						 Smaller 
				   	else if (r1 < r2) then
						Greater
					else 
						Equal
	|Comput ->  if (r1 < r2) then
						 Smaller 
				   	else if (r1 > r2) then
						Greater
					else 
						Equal
;;


let worst_for playeur = match playeur with
			|Human -> (-5)
			| Comput -> (5)				
;; 


(*Attention pas le notre*)
let best_for p = match p with
			|Human -> (5)
			| Comput -> (-5)