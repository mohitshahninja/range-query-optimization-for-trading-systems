


(* Segment Tree for Range Sum Query *)

type segment_tree =
  | Leaf of int
  | Node of int * segment_tree * segment_tree

let rec build arr l r =
  if l = r then Leaf arr.(l)
  else
    let m = (l + r) / 2 in
    let left = build arr l m in
    let right = build arr (m + 1) r in
    match (left, right) with
    | (Leaf x, Leaf y) -> Node (x + y, left, right)
    | (Node (x, _, _), Node (y, _, _))
    | (Leaf x, Node (y, _, _))
    | (Node (x, _, _), Leaf y) -> Node (x + y, left, right)

let rec query tree l r ql qr =
  match tree with
  | Leaf v -> v
  | Node (v, left, right) ->
    if ql <= l && r <= qr then v
    else
      let m = (l + r) / 2 in
      let res = ref 0 in
      if ql <= m then res := !res + query left l m ql qr;
      if qr > m then res := !res + query right (m + 1) r ql qr;
      !res

(* Example usage *)
let () =
  let arr = [|1; 3; 5; 7; 9; 11|] in
  let n = Array.length arr in
  let tree = build arr 0 (n - 1) in
  Printf.printf "Sum of values in range [1, 3] is %d\n" (query tree 0 (n - 1) 1 3)
