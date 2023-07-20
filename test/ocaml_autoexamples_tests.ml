open Core
open Ocaml_autoexamples

let%expect_test "trivial_test" =
  let f () = printf "%s" "trivial test" in
  printf "%s" (Collector.output_of f);
  [%expect {| trivial test |}]
