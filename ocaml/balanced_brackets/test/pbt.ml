open OUnit2
open QCheck
open Balanced_bracket
   
let rec gen_balanced n =
  let open Gen in 
  match n with
    0 -> Gen.string_size ~gen:(Gen.char_range 'a' 'z') (Gen.int_range 1 5)
  | _->  oneof [
             (gen_balanced (n-1)) >>= (fun x-> return ("["^x^"]"));
             (gen_balanced (n-1)) >>= (fun x-> return ("("^x^")"));
             (gen_balanced (n-1)) >>= (fun x-> return ("{"^x^"}"))
           ]

let gen_unbalanced n =
  let t = gen_balanced n in 
  let open Gen in 
  let p = oneof [
             return "[";
             return "(";
             return "{";
             return "]";
             return ")";
             return "}"
           ]
    in 
    p  >>= fun x-> (t >>= (fun y -> return (x^y)))

let  test_balanced_bracket = 
    Test.make
 ~name:"test_balanced_bracket"
 ~count:100 (* number of tests *)
 (int_range 1 10) (* depth of the breackets *)
 (fun n ->
   let t = Gen.generate1 (gen_balanced n) in 
   (*let _= Printf.printf "%s\n"t in*)
   balanced t
 )

let  test_unbalanced_bracket = 
    Test.make
 ~name:"test_unbalanced_bracket"
 ~count:100 (* number of tests *)
 (int_range 1 10) (* depth of the breackets *)
 (fun n ->
   let t = Gen.generate1 (gen_unbalanced n) in 
   (*let _= Printf.printf "Test: %s\n"t in*)
   not (balanced t)
 )
  
let suite =
  "pbt"
  >:::[
      QCheck_runner.to_ounit2_test test_balanced_bracket;
      QCheck_runner.to_ounit2_test test_unbalanced_bracket
    ]
let _ = run_test_tt_main suite
