open Core
open Graph

module Node   = struct
  type t = {id:int;label:string}
  let hash = Hashtbl.hash
  let equal = (=)
  let create id label = {id; label}
  let id n = n.id
  let compare x y = compare x.id y.id
end


(* representation of an edge -- must be comparable *)
module Edge = struct
  type t = ((int, Int.comparator_witness) Set.t) ref
  let to_string e = "[" ^ String.concat ~sep:"," (List.map ~f:(string_of_int) (Set.to_list (!e))) ^ "]"
  let compare e1 e2=
    let e1 = !e1 in
    let e2 = !e2 in
    Set.compare_direct e1 e2
  let add e lbl = ref (Set.add (!e) lbl)
  let equal = (=)
  let default = ref (Set.empty (module Int))
end

module G = Imperative.Digraph.ConcreteLabeled(Node)(Edge)

module Dot = Graph.Graphviz.Dot(struct
                 include G (* use the graph module from above *)
                 let edge_attributes (_, e, _) = [`Label (Edge.to_string e); `Color 4711]
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
  let v1 = G.V.create {id=1000;label="A"} in
  let v2 = G.V.create {id=1001;label="B"} in
  let e1 = Edge.default in
  let e1 = Edge.add e1 1 in
  let e1 = Edge.add e1 2 in
  let e = G.E.create v1 e1  v2 in
  G.add_edge_e g e;
  let e2 = Edge.default in
  let e2 = Edge.add e2 3 in
  let e2 = Edge.add e2 4 in
  let e = G.E.create v2 e2  v1 in
  G.add_edge_e g e;
  g_to_dot g "imperative_graph.dot"


let _ = main ();;
