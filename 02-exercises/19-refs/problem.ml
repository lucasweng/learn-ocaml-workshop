open! Base

(* It is sometimes useful to create a single mutable value. We can do this using
   a [ref]. We can create an [int ref] containing 0 as follows. *)
let x = ref 0

(* Then we can access the value in the ref using the [!] operator, and we can
   update it using the [:=] operator. So, we could increment our ref as
   follows. *)
let () =
  x := !x + 1

(* Write a function min_and_max which returns a tuple containing the minimum and
   maximum values in a non-empty list of positive integers. Your function should
   raise if the list is empty.

   You could do this using [List.fold], but for the purpose of this exercise,
   let's iterate over the list and explicitly maintain refs of the minimum and
   maximum values seen so far instead. *)
exception Empty_list

let min_and_max lst =
  match List.hd lst with
  | None -> raise Empty_list
  | Some n ->
    let rec aux (min, max) l =
      match l with
      | [] -> (!min, !max)
      | x :: xs ->
        if x < !min then min := x
        else if x > !max then max := x;
        aux (min, max) xs
    in
    aux (ref n, ref n) lst

(* By the way, can you guess how a [ref] is implemented under the hood? 

   (Hint: exercise 18.) *)

module RefImpl = struct
  type 'a t = {
    mutable contents : 'a;
  }
  let ref x = { contents = x }
  let (!) x = x.contents
  let (:=) x y = x.contents <- y
end

let%test "Testing min_and_max..." =
  [%compare.equal: int * int] (min_and_max [5;9;2;4;3]) (2,9) 
;;

let%test "Testing min_and_max..." =
  [%compare.equal: int*int] (min_and_max [11;15;7;34]) (7,34)
;;
