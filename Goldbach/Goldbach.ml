module Goldbach=struct 

exception NumberNotAllowedException;;

let rec is_prime n = 
	let rec is_prime n k primeOrNot= match (n-((n/(k))*k)) with (*resto*)
	0 when k==n -> primeOrNot
	| 0 -> false
	| _ -> is_prime n (k+1) (primeOrNot&&true)
	in
	is_prime n 2 true ;;

let rec find_partition n=
	let rec find_partition n k= match (is_prime (fst k),is_prime (snd k)) with
	(true,true) -> k
	| _ -> find_partition n (((fst k) +1),((snd k)-1)) 
	in find_partition n (2,(n-2)) ;;

let rec goldbach(n)= match (n-((n/2)*2)) with
	0 when n>2-> (*allora è pari e posso trovare una partizione*) (find_partition n)
	| _-> (0,0);;

let rec goldbach_list(n,m)=
	let rec goldbach_list (n,m) k l= match (k-m) with
		0 -> List.rev (List.filter (fun x-> x<>(0,0)) (goldbach(k)::l))  
		| _ -> goldbach_list (n,m) (k+1) (List.rev (goldbach(k)::l))
	in goldbach_list (n,m) n []
	

end;;