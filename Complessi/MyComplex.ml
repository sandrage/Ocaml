module MyComplex = struct

type complex = {re: float; i: float} ;;

let create ?(r=0.) imm = {re=r;i=imm} ;;

let (+/) a b = {re=(a.re +. b.re);i=(a.i +. b.i)} ;;

let (-/) a b = {re=(a.re -. b.re);i=(a.i -. b.i)} ;;

let ( */ ) a b = {re=((a.re *. b.re) -. (a.i *. b.i));i=((a.i *. b.re) +. (a.re *. b.i))} ;;

let (//) a b = {re= ((a.re *. b.re) +. (a.i *. b.i)) /. (b.re ** 2. +. (b.i ** 2.)) ; i=((a.i *. b.re -. a.re *. b.i) /. (b.re ** 2. +. b.i ** 2. ))} ;;

let count x =
      match (x -. float_of_int(int_of_float(x))) with
      | 0. -> 0
      | 0.1 | 0.2 | 0.3 | 0.4 | 0.5 | 0.6 | 0.7 | 0.8 | 0.9 -> 1
      | _ -> 2 ;;

    let tostring x= match (x.re,x.i) with
    | (_,0.) -> Printf.sprintf "% .*f" (count (x.re)) (x.re)
    | (0.,_) -> Printf.sprintf "%.*fi" (count x.i) (x.i)
    | (_,_) -> Printf.sprintf "%.*f+%.*fi" (count (x.re)) (x.re) (count (x.i)) (x.i)

end;;