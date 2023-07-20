open Core
open Ocaml_autoexamples
open Example

let%expect_test "trivial_test" =
  let f () = printf "%s" "trivial test" in
  printf "%s" (Collector.output_of f);
  [%expect {| trivial test |}]

let%expect_test "trival write corrected file test" =
  let file_name = "trivial.ml" in
  let file_path = "data/" ^ file_name in
  let example : Example.t =
    {
      function_name = "a_function";
      example_body = "this is a test";
      loc =
        {
          loc_start =
            { pos_fname = file_name; pos_bol = 5; pos_lnum = 2; pos_cnum = 5 };
          loc_end =
            { pos_fname = file_name; pos_bol = 5; pos_lnum = 2; pos_cnum = 5 };
          loc_ghost = false;
        };
    }
  in
  let res =
    match Writer.write_corrected_file file_path [ example ] with
    | Error msg -> msg
    | Ok () -> In_channel.read_all (file_path ^ ".corrected")
  in
  printf "%s" res;
  [%expect {||}]
