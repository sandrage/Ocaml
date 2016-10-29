
module PolishCalculator (S: MyStack.MyStack) = struct
	type expr = Operando of int | Complex of expr * operazione * expr 
		and operazione = Per | Piu | Meno | Diviso | Potenza

	let getPrimaParola stringa = try  
				String.sub stringa 0 (String.index stringa ' ') 
		with e-> stringa ;; 

	let excludePrimaParola stringa = try 
			String.sub stringa ((String.index stringa ' ')+1) ((String.length stringa) - (String.index stringa ' ') -1) 
		with e-> stringa ;;
	
	let rec removeBlanks lista partial= match lista with
		[] -> partial
		| ""::tl -> removeBlanks tl partial
		| h::tl -> h::(removeBlanks tl (partial));;

	let rec split stringa=
		let rec split stringa lista= try 
			if (String.index stringa ' ')>=0 then ((getPrimaParola stringa)::(split (excludePrimaParola stringa) lista))
		else lista 
		with e-> stringa::lista in split stringa [];;

	let rec expr_of_string stringa = 
		let pilaExpr : expr S.t= S.create () in 

		let rec expr_of_string pila = function
			[] -> (try S.pop pila with e-> Operando 0) 
 			| "*"::tl -> (try 
							S.push (Complex (S.pop pila,Per,S.pop pila)) pila;
							expr_of_string pila tl
						with e -> Operando 0)
			| "-"::tl -> (try
							S.push (Complex (S.pop pila, Meno,(S.pop pila))) pila;
							expr_of_string pila tl
						with e-> Operando 0)
			| "+"::tl -> (try
							S.push (Complex (S.pop pila, Piu, (S.pop pila))) pila;
							expr_of_string pila tl
						with e-> Operando 0)
			| "/"::tl -> (try
							S.push (Complex (S.pop pila, Diviso, (S.pop pila))) pila;
							expr_of_string pila tl
						with e-> Operando 0) 
			| "**"::tl -> (try 
							S.push (Complex (S.pop pila, Potenza, (S.pop pila))) pila;
							expr_of_string pila tl
						with e-> Operando 0)

			| h::tl -> 	S.push (Operando (int_of_string h)) pila; 
						expr_of_string pila tl 
		in 
		S.push (Operando 0) pilaExpr;
		expr_of_string  pilaExpr (removeBlanks (split stringa) []) ;;

	let rec eval express =
		let rec eval express = match express with
			 Operando (n) -> n 
			| Complex (c,Piu,(Operando (n))) -> ((eval c) + n)
			| Complex (c,Meno,(Operando (n))) -> ((eval c) - n)
			| Complex (c,Per,(Operando (n))) -> ((eval c) * n)
			| Complex (c,Diviso,(Operando (n))) -> ((eval c) / n)
			| Complex (c,Potenza,(Operando (n))) -> int_of_float((float_of_int (eval c)) ** (float_of_int n)) 
			| Complex ((Operando (n)),Potenza,c) -> int_of_float((float_of_int (n)) ** float_of_int (eval c))
			| Complex (c,Potenza,d) -> int_of_float((float_of_int (eval c)) ** (float_of_int (eval d)))
			| Complex (c,Piu,d) -> (eval c) + (eval d)
			| Complex (c, Meno, d) -> (eval c) - (eval d)
			| Complex (c, Diviso, d) -> (eval c) / (eval d)
			| Complex (c, Per, d) -> (eval c) * (eval d)
		in eval express 

	end;;