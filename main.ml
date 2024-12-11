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

(* tests *)

assert_eq "" "";;
assert_eq 1 1;;
assert_eq (1, 2) (3 - 2, 5 - 3)

(* results *)

let () = printf "%d tests, %d passed and %d failed\n" !tests !passed !failed
