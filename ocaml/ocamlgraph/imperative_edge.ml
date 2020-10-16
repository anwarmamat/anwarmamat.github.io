open Core
open Graph

module Node = struct

  type nodeType = 
    Branch_start
  |Branch_end
  |Flow
  |Block

  type t = {
      id:int;
      label:string;
      mutable node_type:nodeType
    }
  let hash = Hashtbl.hash
  let equal n1 n2 = n1.id = n2.id 
  let create id lbl = {id = id; label = lbl; node_type = Block}
  let setType node t = node.node_type <-t
  let getType node = node.node_type
  let id n = n.id
  let compare x y = Int.compare (x.id) (y.id)
end


(* representation of an edge -- must be comparable *)
module Label = struct
  type t = ((int, Int.comparator_witness) Set.t) ref
  let to_string e = "[" ^ String.concat ~sep:"," (List.map ~f:(string_of_int) (Set.to_list (!e))) ^ "]"
  let compare e1 e2=
    let e1 = !e1 in
    let e2 = !e2 in
    Set.compare_direct e1 e2
  let add edge lbl = ref (Set.add (!edge) lbl)
  let remove edge lbl = ref (Set.remove (!edge) lbl)
  let mem edge lbl = Set.mem (!edge) lbl
  let equal = (=)
  let default = ref (Set.empty (module Int))
end

module G = Imperative.Digraph.ConcreteLabeled(Node)(Label)

module Dot = Graph.Graphviz.Dot(struct
                 include G (* use the graph module from above *)
                 let edge_attributes (_, e, _) = [`Label (Label.to_string e); `Color 4711]
                 let default_edge_attributes _ = []
                 let get_subgraph _ = None
                 let vertex_attributes v = [`Shape `Box ;`Fontsize 24;`Label (G.V.label v).label]
                 let vertex_name v = (string_of_int (G.V.label v).id)
                 let default_vertex_attributes _ = []
                 let graph_attributes _ = [`OrderingOut; `Rankdir `TopToBottom; `Orientation `Landscape]
               end)
let g_to_dot gr filename=
  let file = Caml.open_out_bin filename in
  Dot.output_graph file gr

let main () =
  let g = G.create () in
  let v1 = Node.create 1000 "A" in
  let v2 = Node.create 1001 "B" in
  let e1 = Label.default in
  let e1 = Label.add e1 1 in
  let e1 = Label.add e1 2 in
  let e = G.E.create v1 e1  v2 in
  G.add_edge_e g e;
  let e2 = Label.default in
  let e2 = Label.add e2 3 in
  let e2 = Label.add e2 4 in
  let e2 = Label.add e2 5 in
  let e2 = Label.remove e2 4 in
  let e = G.E.create v2 e2  v1 in
  G.add_edge_e g e;
  g_to_dot g "imperative_graph.dot"


let _ = main ();;
