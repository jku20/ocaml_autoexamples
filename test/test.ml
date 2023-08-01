let gen () = ()

(** this is some junk woah 
    -1-2-5-3-9-----EXAMPLE-----1-0-9-7-3-
    function "a_function" prints: this is a test
    -1-2-5-3-9-----------------1-0-9-7-0-*)
let%autoex a_function () = "this is a test" [@@autoex gen, Fun.id]
