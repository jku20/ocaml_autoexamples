open Core
open Runner
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
  [%expect
    {|
    (** this is some junk woah
        -1-2-5-3-9-----EXAMPLE-----1-0-9-7-3-
        function "a_function" prints: this is a test
        -1-2-5-3-9-----------------1-0-9-7-0-*)
    let a_function = Printf.printf "this is a test"

    (** this is some junk woah
        -1-2-5-3-9-----EXAMPLE-----1-0-9-7-3-
        function "a_function" prints: this is a test
        -1-2-5-3-9-----------------1-0-9-7-0- *)
    let b_function = Printf.printf "this is another test" |}]

let%expect_test "trival replace corrected file test" =
  let file_name = "trivial.ml" in
  let file_path = "data/" ^ file_name in
  let example : Example.t =
    {
      function_name = "b_function";
      example_body = "this is another test";
      loc =
        {
          loc_start =
            { pos_fname = file_name; pos_bol = 5; pos_lnum = 8; pos_cnum = 5 };
          loc_end =
            { pos_fname = file_name; pos_bol = 5; pos_lnum = 8; pos_cnum = 5 };
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
  [%expect
    {|
    (** this is some junk woah *)
    let a_function = Printf.printf "this is a test"

    (** this is some junk woah
        -1-2-5-3-9-----EXAMPLE-----1-0-9-7-3-
        function "b_function" prints: this is another test
        -1-2-5-3-9-----------------1-0-9-7-0- *)
    let b_function = Printf.printf "this is another test" |}]
