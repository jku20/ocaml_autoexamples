open Ppxlib

let expand ~ctxt _p _e =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  [%stri let () = Printf.printf "hello"]

let extension =
  let pat =
    Ast_pattern.(
      pstr @@ pstr_value drop (value_binding ~pat:__ ~expr:__ ^:: nil) ^:: nil)
  in
  Extension.V3.declare "autoex" Extension.Context.structure_item pat expand

let rule = Ppxlib.Context_free.Rule.extension extension
let () = Driver.register_transformation ~rules:[ rule ] "autoex"
