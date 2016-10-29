open MyComplex.MyComplex ;;

let main () =
  let a = create ~r:6. 7. and
      b = create ~r:3.5 (-8.)
  in let c = a +/ b in
       let d = (create ~r:(-9.5) 0.) +/ c in
         Printf.printf "a :- %s\n" (tostring a);
         Printf.printf "b :- %s\n" (tostring b);
         Printf.printf "c = a+b :- %s\n" (tostring c);
         Printf.printf "d = -9.5+c :- %s\n" (tostring d);
         let e = a -/ b and f = (create ~r:7. 0.) -/ b in 
           Printf.printf "e = a-b :- %s\n" (tostring e);
           Printf.printf "f = 7-b :- %s\n" (tostring f);
           let g = e */ f in  
             Printf.printf "g = e*f :- %s\n" (tostring g);
             let h = (create ~r:7. 0.) */ g and 
                 i = (create (-1.)) */ (create (-1.)) and
                 j = a // g and
                 k = (create ~r:(-1.) 0.) // a in
               Printf.printf "h = 7*g :- %s\n" (tostring h);
               Printf.printf "i = -i*-i :- %s\n" (tostring i);
               Printf.printf "j = a/g :- %s\n" (tostring j);
               Printf.printf "k = -1/a :- %s\n" (tostring k);
               let z = a*/(b+/c)-/d*/(e+/f-/g)//(h+/i*/j) in 
                 Printf.printf "z :- %s\n" (tostring z);;

let() = main() ;;