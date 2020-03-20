type tree = Leaf of int | Fork of tree * tree

let rec cost t = match t with
| Leaf(x) -> x
| Fork(t1,t2) -> 1 + (max (cost t1) (cost t2))
;;

let rec map f l = match l with
| [] -> []
| x::xs -> (f x)::(map f xs)

let rec append x y = match x with
| [] -> y
| x::xs -> x::(append xs y)
;;

let rec concat l = match l with
| [] -> []
| x::xs -> append x (concat xs)
;;

let rec prefixes i t = match t with
| Leaf(x) -> [Fork(Leaf(i),t)]
| Fork(u,v) -> append [Fork(Leaf(i),t)] 
                      (map (fun a -> Fork(a,v)) (prefixes i u))
;;

let rec trees l = match l with
| [] -> []
| x::[] -> [Leaf(x)]
| x::xs -> concat (map (prefixes x) (trees xs))
;;
