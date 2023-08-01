let gen () = ()

(** this is some junk woah *)
let%autoex a_function () = Printf.printf "this is a test"
[@@autoex_gen gen]
