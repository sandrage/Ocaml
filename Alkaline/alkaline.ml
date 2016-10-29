let alkaline_earth_metals = [4;12;20;38;56;88];;

let rec highest_atomic_number =
  let rec highest_atomic_number acc = function
        [] -> acc
      | h::tl -> if (h > acc) then highest_atomic_number h tl else highest_atomic_number acc tl in
      highest_atomic_number 0 alkaline_earth_metals ;; (*versione elaborata*)

let highest_easy =
    let sorted = List.sort (fun x y -> y-x) alkaline_earth_metals in
    List.hd sorted ;; (*scorciatoia*)

	
let sorted = List.sort (fun x y-> x-y) alkaline_earth_metals ;; (*scorciatoia*)


let rec sorted_elaborata = 
	let rec sorted_elaborata = function 
	[] -> []
  | h::tl -> sorted_elaborata (List.filter (fun x -> x < h) tl) @ [h] @ (List.filter (fun x -> x > h) tl ) in sorted_elaborata alkaline_earth_metals ;; (*elaborata*)
  
let noble_gases = [2;10;18;36;54;86];;
let names_gases = ["helium";"neon";"argon";"krypton";"xenon";"radon"];;

(*Costruire le coppie (nome,valore_gas) ordinati per nome*)
let rec create_couples = 
	let rec create_couples l1 l2= match (l1,l2) with 
	  ([],[]) | ([],_) | (_,[]) -> []
	| (h1::tl1,h2::tl2) -> (h1,h2) :: create_couples tl1 tl2 in 
	create_couples names_gases noble_gases ;;

let rec sorting_couples =
	let rec sorting_couples = function 
		[] -> []
	 | (h1,h2)::tl -> sorting_couples (List.filter (fun (x,y) -> x < h1) tl) @ [(h1,h2)] @ sorting_couples (List.filter (fun (x,y) -> x > h1) tl) in 
	sorting_couples create_couples ;;

  
 