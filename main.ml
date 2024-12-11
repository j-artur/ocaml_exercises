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

(* results *)

let () = printf "%d tests, %d passed and %d failed\n" !tests !passed !failed
