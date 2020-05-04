(*
21. Merge Two Sorted Lists

Merge two sorted linked lists and return it as a new list. The new list should be made by splicing together the nodes of the first two lists.

Example:

Input: 1->2->4, 1->3->4
Output: 1->1->2->3->4->4
 *)

let l1 = [1;2;2;4];;
let l2 = [1;3;4;5];;

let rec merge lst1 lst2 =
  match lst1,lst2 with
    [],[]->[]
   |a,[] -> a
   |[],a -> a
   |(h1::t1,h2::t2) -> if h1 < h2 then h1::(merge t1 lst2)
                     else h2::(merge  lst1 t2)
;;


let l3 = merge l1 l2;;
