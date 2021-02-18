(*

#use "topfind";;
#require "sexplib";;
#require "ppx_sexp_conv";;
#require "ppx_deriving.std";;
*)


(* to compile:
1) comment above "require"s
2) compile with 

ocamlfind ocamlc -linkpkg -package Ppx_deriving.std,Ppx_sexp_conv,Sexplib ppx_deriving_test.ml

 *)
(*
#require "ppx_deriving.show";;
#require "ppx_deriving.eq";;
#require "ppx_deriving.fold";;
#require "ppx_deriving.iter";;
#require "ppx_deriving.map";;
#require "ppx_deriving.std";;
#require "sexplib";;
#require "ppx_sexp_conv";;
open Ppx_deriving_iter;;
open Ppx_deriving_fold;;
open Ppx_deriving_map;;



*)

open Ppx_deriving_std;;
open Sexplib;;
open Sexplib.Conv;;
open Sexplib.Sexp;;
open Ppx_sexp_conv_lib;;
open Format;;

type t = int * int [@@deriving sexp, show];;
let a = (10,20 [@opaque]);;
let s = sexp_of_t a;;
Printf.printf "%s\n" (show (t_of_sexp s));;

type point = {x:float; y:float; color:string} [@@deriving sexp, show, eq];;
let p1 = {x=100.;y=200.; color="red"};;
let p2 = {x=100.;y=200.; color="red"};;
Printf.printf "%s\n" (show_point p1);;
                                      
(*
 type 'a tree = Leaf | Node of 'a tree * (int [@printer fun fmt -> fprintf fmt "0o%03o"]) * 'a tree
[@@deriving show, sexp, iter, map, fold];;
 *)
(*[@opaque] is a shorthand for [@printer fun fmt _ -> Format.pp_print_string fmt "<opaque>"]*)

type 'a tree = Leaf | Node of 'a tree * 'a  * 'a tree [@@deriving show, sexp, iter, map, fold];;

let t1 = Node(Node (Leaf, 50, Leaf), 100, Node(Leaf, 200, Leaf));;

(*sexp_of_tree (fun x-> Sexplib0.Sexp.Atom x) t1;;*)

(*Printf.printf "%s\n" (Sexp.to_string (sexp_of_tree t1));;*)

Printf.printf "%s\n" (show_tree (fun ppf y->fprintf ppf "%d" y) t1);;

Printf.printf "sum=%d\n" (fold_tree (+) 0 t1)
;;
