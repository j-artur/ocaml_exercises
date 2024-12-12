open Printf

(* tests *)

let tests = ref 0
let passed = ref 0
let failed = ref 0

let assert_eq (a : 'a) (b : 'a) : unit =
  let () = tests := !tests + 1 in
  if a = b
  then (
    let () = passed := !passed + 1 in
    printf "test %d passed\n" !tests)
  else (
    let () = failed := !failed + 1 in
    eprintf "test %d failed\n" !tests)
;;

(* 1. Tail of a List *)

let rec last (list : 'a list) : 'a option =
  match list with
  | [] -> None
  | [ e ] -> Some e
  | _ :: tail -> last tail
;;

assert_eq (last [ "a"; "b"; "c"; "d" ]) (Some "d");;
assert_eq (last []) None

(* 2. Last Two Elements of a List *)

let rec last_two (list : 'a list) : ('a * 'a) option =
  match list with
  | [] | [ _ ] -> None
  | [ x; y ] -> Some (x, y)
  | _ :: tail -> last_two tail
;;

assert_eq (last_two [ "a"; "b"; "c"; "d" ]) (Some ("c", "d"));;
assert_eq (last_two [ "a" ]) None

(* 3. N'th Element of a List *)

let rec at n list =
  match n, list with
  | _, [] -> None
  | 0, head :: _ -> Some head
  | n, _ :: tail -> at (n - 1) tail
;;

assert_eq (at 2 [ "a"; "b"; "c"; "d"; "e" ]) (Some "c");;
assert_eq (at 2 [ "a" ]) None

(* 4. Length of a List *)

let rec length = function
  | [] -> 0
  | _ :: tail -> length tail + 1
;;

assert_eq (length [ "a"; "b"; "c" ]) 3;;
assert_eq (length []) 0

(* 5. Reverse a List *)

let rec rev list =
  let rec rev_help list acc =
    match list with
    | [] -> acc
    | hd :: tail -> rev_help tail (hd :: acc)
  in
  rev_help list []
;;

assert_eq (rev [ "a"; "b"; "c" ]) [ "c"; "b"; "a" ]

(* 6. Palindrome *)

let is_palindrome list =
  let rec is_palindrome_help list rev_list =
    match list, rev_list with
    | [], [] -> true
    | hd1 :: tail1, hd2 :: tail2 when hd1 = hd2 -> is_palindrome_help tail1 tail2
    | _ -> false
  in
  is_palindrome_help list (rev list)
;;

assert_eq (is_palindrome [ "x"; "a"; "m"; "a"; "x" ]) true;;
assert_eq (not (is_palindrome [ "a"; "b" ])) true

(* results *)

let () = printf "%d tests, %d passed and %d failed\n" !tests !passed !failed
