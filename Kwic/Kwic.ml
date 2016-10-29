
module Kwic= struct
type line={mutable num_riga: int; mutable before_keyword: string; mutable after_keyword: string} ;;
type index={mutable num_righe: int; mutable line: line list};;

let main()={num_righe=0;line=[]};;
let lunghezza stringa=String.length stringa;;
let getPrimaParola stringa=try
		String.sub stringa 0 (String.index stringa ' ')
	with e-> stringa;;

let excludePrimaParola stringa=String.sub stringa (String.length (getPrimaParola stringa)) ((lunghezza stringa)-(String.length (getPrimaParola stringa)));;

let rec adjust_line line num_line=
	let rec adjust_line line adjusted_line i= try
		if ((String.index line ' ')>=0) then 
				begin
				adjust_line (String.trim(excludePrimaParola line)) ((String.trim(getPrimaParola line))::adjusted_line) 0
			end
		else
			adjust_line line adjusted_line (i+1)
		with e-> 
			List.rev (line::adjusted_line)
	in adjust_line (String.trim(line)) [] 0;;

(*creare per ogni parola una linea*)
(*mettere insieme tutte le linee e riordinarle in base alla keyword registrata*)
let create_line num before after = {num_riga=num; before_keyword=String.concat " " before; after_keyword=String.concat " " after};;

let rec sort_index index={num_righe=index.num_righe;line=(List.sort (fun x y-> String.compare (String.lowercase x.after_keyword) (String.lowercase y.after_keyword)) index.line)};;

let rec insert_into_index titolo index=
	let rec insert_into_index primaParte secondaParte righeIndex numRighe=match (secondaParte) with
	[] -> {num_righe=numRighe;line=List.rev(righeIndex)}
	|h::tl when (String.length h)<3 || (String.lowercase h)="the" || (String.lowercase h)="and" -> insert_into_index (h::primaParte) (tl) righeIndex numRighe 
	|h::tl -> insert_into_index (h::primaParte) (tl) ((create_line numRighe (List.rev primaParte) (h::tl))::righeIndex) (numRighe)
	in
	insert_into_index [] titolo index.line (index.num_righe+1);;
	
let rec create_rows listaTitoli=
	let rec create_rows listaTitoli index=match listaTitoli with
	[]-> sort_index index
	| h::tl -> create_rows tl (insert_into_index h index)
	in
	create_rows listaTitoli {num_righe=0;line=[]} ;; 

let rec print_index index=
	let rec print_index linee=match linee with
	[] -> Printf.printf "%s" ""
	| h::tl -> begin
				Printf.printf "%*i " 5 h.num_riga;
				Printf.printf "%*s " 33 (if (String.length h.before_keyword)>33 then (String.sub h.before_keyword 0 33) else h.before_keyword);
				Printf.printf "%-*s" 40 (if (String.length (h.after_keyword^".") >40) then (String.sub (h.after_keyword^".") 0 40) else (h.after_keyword^"."));
				Printf.printf "%s\n" "";
			  	print_index tl
			  end
	in print_index index.line;;

let rec read_line file= 
	let rec read_line listaTitoli opened numrighe=
	try
	read_line ((adjust_line (input_line opened) (numrighe))::listaTitoli) opened (numrighe+1)
	with e->
			close_in_noerr opened;
			print_index (create_rows (List.rev listaTitoli)) in 
	read_line [] (open_in file) 1
end;;