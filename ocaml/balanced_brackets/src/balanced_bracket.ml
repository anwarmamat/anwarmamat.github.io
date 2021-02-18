open String
   
let balanced str =
  let pair left right =
    match (left,right) with
    |'(', ')' -> true
    |'[', ']' -> true
    |'{', '}' -> true
    |_-> false
  in 

  let s = Stack.create () in 
  let letters = String.to_seq str in
  let ret  = Seq.fold_left (
                 fun acc letter ->
                 let r = match letter with
                   |'('
                    |'{'
                    |'[' -> let () = Stack.push letter s in true
                   |')'
                    |'}'
                    |']' -> if Stack.is_empty s then false
                            else let left = Stack.pop s in
                                 pair left letter 
                   |_-> true
                 in 
                 r &&  acc
               ) true letters
  in
  ret && (Stack.is_empty s)    
;;

assert (balanced "()" = true);;
assert (balanced ")(" = false);;
assert (balanced "[()]" = true);;
assert (balanced "" = true);;
assert (balanced "([)]" = false);;
assert (balanced "1{2[3(a)4]5}6" = true);;
assert (balanced "[zkqya" = false);;
assert (balanced "}{g}" = false);;

