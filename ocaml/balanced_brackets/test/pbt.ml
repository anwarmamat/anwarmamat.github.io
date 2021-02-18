open OUnit2
open QCheck
open Balanced_bracket
   
let rec gen_balanced n =
  let open Gen in 
  match n with
    0 -> Gen.string_size ~gen:(Gen.char_range 'a' 'z') (Gen.int_range 1 5)
  | _->  let n1 = Random.int n in
         let n2 = Random.int n in
         let left = (gen_balanced n1) in
         let right = (gen_balanced n2) in 
         oneof [
             left >>= (fun l ->
               (gen_balanced (n-1)) >>= (fun x ->
                         right >>= (fun r ->
                                           return (l ^ "[" ^ x ^ "]" ^ r))));

             left >>= (fun l ->
               (gen_balanced (n-1)) >>= (fun x ->
                         right >>= (fun r ->
                                           return (l ^ "(" ^ x ^ ")" ^ r))));

             left >>= (fun l ->
               (gen_balanced (n-1)) >>= (fun x ->
                         right >>= (fun r ->
                                           return (l ^ "{" ^ x ^ "}" ^ r))))
           ]

let gen_unbalanced n =
  let t = gen_balanced n in
  let _= Random.self_init ()  in 
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
  t  >>= fun x-> (p >>= (fun y ->
                    let len = String.length x in 
                    let i = Random.int len in 
                    let s = (String.sub x 0 i) ^ y ^ (String.sub x  i (len-i)) in
                    return s
                  ))


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
