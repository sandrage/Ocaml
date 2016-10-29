let rec factorial x=
	if x=0 || x=1 then 1
	else x * ( factorial (x-1) ) ;;
let rec expon b e= (float_of_int b) ** (float_of_int e);;


let rec sen x n = 
	if n=0 then float_of_int x 
	else ( ( expon (-1) n ) *. (expon x (2*n+1)) /. (float_of_int (factorial (2 * n + 1))) ) +. (sen x (n-1));; 
