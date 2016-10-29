module SocialNetwork= struct
	type graph={mutable nodes: string list; mutable links: ((string * string) * label) list}
		and  label = Friendship | Kinship | FinancialExchange | Dislike | SRelationship | Beliefs | Knowledge | Prestige ;;
		let empty () = {nodes=[] ; links=[]} ;;
		
		let rec is_present_user v g=
		let rec is_present_user v g g'=match g' with
			[] -> false
			| h::tl when h=v -> true 
			| h::tl -> is_present_user v g tl 
		in is_present_user v g g.nodes ;;
		exception UserNotFound ;;

	let insert_user v g= match (is_present_user v g) with 
		true -> g 
		| false -> (g.nodes <- (v::g.nodes)); g ;;

	let rec find_friends a g = List.map snd (List.map fst (List.filter ( fun x-> (fst (fst x))=a) g.links )) ;;

	let rec is_present_friendship a b g type_rel= match (List.filter (fun x-> (fst (fst x))=a && (snd (fst (x)))=b && (snd x)=type_rel) g.links ) with
			[] -> false
		| _ -> true ;;

	let rec add_friend a b g type_relation= (*aggiungere b agli amici di a e viceversa*) 
		match (is_present_friendship a b (insert_user b (insert_user a g)) type_relation) with
			false ->begin 
					g.links <-(((a,b),type_relation)::g.links);
					g.links <-(((b,a),type_relation)::g.links);
					g
					end			
			|true -> g 				  
			;;
	let remove_friendship a b g type_rel= (g.links<-(List.filter (fun x-> ((fst (fst(x)) <> a || (snd(fst x)) <> b || (snd x)<>type_rel) && ((fst (fst x)) <> b || (snd(fst x)) <> a || (snd x)<>type_rel) )) g.links)) ; g;;

	let remove_friendships a g = g.links <- (List.filter (fun x-> (fst (fst x)) <> a && (snd(fst x)) <> a) g.links) ; g;;

	let remove_node a g = g.nodes <- (List.filter (fun x-> x <> a) g.nodes) ; g;;

	let remove_user a g = match (is_present_user a g) with 
		true -> remove_node a (remove_friendships a g)
		| false -> g
	;; 

	let rec visit_users g =
		let rec visit_users g visited notVisited = match notVisited with 
			[] -> visited
			| h::tl when ((List.exists (fun x-> x=h) visited)=false) -> visit_users g (visit_users g (h::visited) (find_friends h g)) tl
			| h::tl -> visit_users g (visited) tl 
		in 
			visit_users g [] g.nodes ;; 

end;;