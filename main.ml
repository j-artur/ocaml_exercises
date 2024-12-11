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

assert_eq (last []) None;;
assert_eq (last [ "a"; "b"; "c" ]) (Some "c")

(* 2. Last Two Elements of a List *)

let rec last_two (list : 'a list) : ('a * 'a) option =
  match list with
  | [] | [ _ ] -> None
  | [ x; y ] -> Some (x, y)
  | _ :: tail -> last_two tail
;;

assert_eq (last_two []) None;;
assert_eq (last_two [ 1 ]) None;;
assert_eq (last_two [ 1; 2; 3; 4 ]) (Some (3, 4))

(* results *)

let () = printf "%d tests, %d passed and %d failed\n" !tests !passed !failed
