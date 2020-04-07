module Fraction =
  struct
    (* 2 tuple to represent the numerator and the denominator of the fraction *)
    type fraction  = Frac of int * int 
    exception BadFrac

    let rec gcd (a,b) =
      let (a,b) = if a >= b then (a,b)
                  else (b,a) in
      if b = 0 then a
      else gcd(b,a mod b)
     
    let reduce (Frac(x,y)) =
      let d = gcd(x,y) in
      Frac((x/d), (y/d)) 

    let make_frac (x,y) =
      if y = 0 then raise BadFrac (* denominator cannot be zero *)
      else reduce(Frac(x,y))
      
    let add (Frac(n1,d1),Frac(n2,d2)) =  reduce (Frac (n1 * d2 + d1 * n2, d1 * d2))
                                   
    let toString (Frac(a,b)) = if b = 1 then string_of_int a
                               else if a = 0 then "0"
                               else (string_of_int a) ^ "/" ^ (string_of_int b)
  end;;


let f1 = Fraction.make_frac(4,8);;
let f2 = Fraction.make_frac(50,100);;
print_endline (Fraction.toString (Fraction.add (f1,f2)));;
 
(* because the implementation is visible to user, user may bypass make_frac and try *) 
let f3 = Fraction.Frac (100,200);;
(* then f3 will not be reduced *) 
print_endline (Fraction.toString f3);;

(* To fix this, we should hide the type of fraction using an interface *)
