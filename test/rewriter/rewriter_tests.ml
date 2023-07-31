let gen () = "world"
let%autoex foo s = (Printf.printf "hello %s" s [@autoex_gen gen])
