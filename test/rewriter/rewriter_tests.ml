let gen () = "world"

(** woah *)
let%autoex foo s = Printf.sprintf "hello %s" s [@@autoex gen, Fun.id]
