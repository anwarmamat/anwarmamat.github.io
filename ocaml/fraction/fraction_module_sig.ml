module type FRACTION =
  sig
    type fraction (* hide the type *)
    exception BadFrac
    (*val gcd : int * int -> int*) (* gcd is not visible outside the module *)
    val reduce : fraction -> fraction
    val make_frac : int * int -> fraction
    val add : fraction * fraction -> fraction
    val toString : fraction -> string
  end;;
  

module Fraction : FRACTION =
  struct
    (* 2 tuple to denote the numerator and the denominator of the fraction *)
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
      
    let add (Frac(n1,d1),Frac(n2,d2)) = reduce (Frac(n1 * d2 + d1 * n2, d1 * d2))
                                   
    let toString (Frac(a,b)) = if b = 1 then string_of_int a
                               else if a = 0 then "0"
                               else (string_of_int a) ^ "/" ^ (string_of_int b)
  end;;


let f1 = Fraction.make_frac(4,9);;
let f2 = Fraction.make_frac(50,100);;
print_endline (Fraction.toString (Fraction.add (f1,f2)));;
 
(* now user cannot do this *)
(*
  let f3 = Fraction.Frac (100,200); 
*)


