module type TYPE =                                                
  sig                                                             
    type t                                                        
    val to_str: t->string                                                       
  end

module IntItem : (TYPE with type t = int) =
  struct
    type t = int
    let to_str = string_of_int
  end

module type BAG =
  sig
    type elt
    type t
    val empty :t
    val insert : elt -> t -> t
      
  end
                  

module ListBag  (Elt:TYPE):(BAG with type elt=Elt.t)  = 
  struct
    type elt = Elt.t
    type t = Elt.t list  
    let empty  = []
    let insert x bag = x::bag
    let want_to_hide_this () = print_string "please hide\n"
  end

module M1 = ListBag(IntItem);;


M1.want_to_hide_this ();;



