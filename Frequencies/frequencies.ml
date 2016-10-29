(*LETTURA STRINGHE DA FILE*)

let chan = open_in "fileDaLeggere.txt" ;;

type paroleConFreq ={parola: string; mutable frequenza: int};;
let righe : string list= [] ;;
let frequencies : paroleConFreq list =[];;
let cercaVuoti t=String.index t ' ';;
let excludePrimaParola t=String.sub t ((cercaVuoti t)+1) (((String.length t)-1)-(cercaVuoti t)) ;;
let primaParola t= String.sub t 0 ((cercaVuoti t));;

let rec readingwords = 
	let rec readingwords righe=
		try
		readingwords ((input_line chan)::righe) 
		with e ->
		close_in_noerr chan;
		righe 
		in readingwords righe;;
		
let rec countFrequencies parola listeFreq=match listeFreq with	
	 [] -> {parola=parola;frequenza=0}::listeFreq  
	| h::tl -> if ((String.lowercase h.parola) = (String.lowercase parola)) then begin
						(h.frequenza <- (h.frequenza+1));
						listeFreq
						end
				else h::countFrequencies parola tl ;;

let rec split testo freq=
	try 
		if ((String.index testo ' ')>=0) then 
			split (excludePrimaParola testo) (countFrequencies (primaParola testo) freq)
		else 
			({parola=testo; frequenza=0}::freq)
	with e -> ({parola=testo; frequenza=0}::freq);;
	
let rec constructList righe=
	let rec constructList =function 
		 []->[]
		 | h::tl -> List.append (split h frequencies) (constructList tl) in
		 constructList righe;;

	
	
	