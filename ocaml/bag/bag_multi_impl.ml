open Printf;;

module type Stringable =                                                
  sig                                                             
    type t                                                        
    val to_str: t->string                                                       
  end

module IntItem : (Stringable with type t = int) =
  struct
    type t = int
    let to_str = string_of_int
  end

module FloatItem : (Stringable with type t = float) =
  struct
    type t = float
    let to_str = string_of_float
  end

type point ={x:int;y:int};;
module PointItem : (Stringable with type t = point) =
  struct
    type t = point
    let to_str p = "(x=" ^ string_of_int p.x ^ "; y=" ^ string_of_int p.y ^ ")"  
  end

  
module type BAG =
  sig
    type item (* type of the item in the bag *) 
    type t
    val empty : t
    val insert : item-> t -> t
    val size: t -> int
    val iter: (item -> unit)-> t -> unit
    val from_list: item list -> t -> t       (* insert a list of items into the bag *)
    val toString: t-> string (* return the content of the bag as a string *)
  end
                  

(* List based implementation of the BAG *)  
module ListBag  (Elt:Stringable) : (BAG with type item = Elt.t) =
  struct
    type item = Elt.t
    type t = Elt.t list
    let empty  = []
    let insert x bag = x::bag
    let size bag = List.length bag
    let iter f bag  = List.iter f bag
    let from_list lst b = List.fold_left (fun bag item -> insert item bag) b lst
    ;;    
    let toString bag =
      match bag with
        []->"[]"
       |h::t-> "[" ^ (Elt.to_str h) ^
                 (List.fold_left (fun x y -> x ^ "," ^ (Elt.to_str y)) "" t)
                 ^"]" 
  end

(* create a IntBag  module *)  
module IntBag = ListBag(IntItem);;

let b = IntBag.empty;;
let b = IntBag.insert 100 b;;
let b = IntBag.insert 200 b;;
let b = IntBag.from_list [1;2;3;4;5] b;;
printf "%s\n" "IntBag";;
printf "bag size=%d\n" (IntBag.size b);;
printf  "bag:%s\n" (IntBag.toString b)
 
module FloatBag = ListBag(FloatItem);;

let b = FloatBag.empty;;
let b = FloatBag.insert 10.0 b;;
let b = FloatBag.insert 20.0 b;;
let b = FloatBag.from_list [1.5;2.5;3.5;4.5] b;;
printf "%s\n" "FloatBag";;
printf "bag size=%d\n" (FloatBag.size b);;
printf "bag:%s\n" (FloatBag.toString b)
 
module PointBag = ListBag(PointItem);;
let b = PointBag.empty;;

let b = PointBag.insert ({x=1;y=2}) b;;
let b = PointBag.insert ({x=10;y=20}) b;;
let b = PointBag.insert ({x=100;y=200}) b;;
printf "%s\n" "PointBag";;
printf "bag size=%d\n" (PointBag.size b);;
printf "bag:%s\n" (PointBag.toString b)
