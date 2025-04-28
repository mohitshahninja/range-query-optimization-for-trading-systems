(* Segment Tree with Lazy Propagation for Range Updates *)

type segment_tree =
  | Leaf of int
  | Node of int * int * segment_tree * segment_tree  (* sum, lazy value, left, right *)

let rec build arr l r =
  if l = r then Leaf arr.(l)
  else
    let m = (l + r) / 2 in
    let left = build arr l m in
    let right = build arr (m + 1) r in
    Node (arr.(l) + arr.(r), 0, left, right)


let rec propagate tree l r =
  match tree with
  | Leaf _ -> tree
  | Node (sum, lazy_val, left, right) ->
    if lazy_val <> 0 then
      let new_left = propagate left l ((l + r) / 2) in
      let new_right = propagate right ((l + r) / 2 + 1) r in
      Node (sum + lazy_val * (r - l + 1), 0, new_left, new_right)
    else
      tree

let rec update tree l r ul ur val_ =
  match tree with
  | Leaf _ -> tree
  | Node (sum, lazy_val, left, right) ->
    let new_tree = propagate tree l r in
    if ur < l || r < ul then new_tree
    else if ul <= l && r <= ur then
      Node (sum + val_ * (r - l + 1), lazy_val + val_, left, right)
    else
      let m = (l + r) / 2 in
      let new_left = update left l m ul ur val_ in
      let new_right = update right (m + 1) r ul ur val_ in
      Node (sum + val_ * (r - l + 1), lazy_val, new_left, new_right)

let rec query tree l r ql qr =
  match tree with
  | Leaf v -> v
  | Node (sum, lazy_val, left, right) ->
    let new_tree = propagate tree l r in
    if ql <= l && r <= qr then sum
    else
      let m = (l + r) / 2 in
      let left_res = if ql <= m then query left l m ql qr else 0 in
      let right_res = if qr > m then query right (m + 1) r ql qr else 0 in
      left_res + right_res

(* Example usage *)
let () =
  let arr = [|1; 3; 5; 7; 9; 11|] in
  let n = Array.length arr in
  let tree = build arr 0 (n - 1) in
  let tree_with_update = update tree 0 (n - 1) 1 3 5 in
  Printf.printf "Sum of values in range [1, 3] after update is %d\n" (query tree_with_update 0 (n - 1) 1 3)

