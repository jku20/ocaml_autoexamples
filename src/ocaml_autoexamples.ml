open Ppxlib

let expand ~ctxt flg pat expr =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let (module B) = Ast_builder.make loc in
  let open B in
  pstr_value flg (value_binding ~pat ~expr :: [ value_binding ~pat:punit ~expr ])

let extension =
  let pat =
    Ast_pattern.(
      pstr @@ pstr_value __ (value_binding ~pat:__ ~expr:__ ^:: nil) ^:: nil)
  in
  Extension.V3.declare "autoex" Extension.Context.structure_item pat expand

let rule = Ppxlib.Context_free.Rule.extension extension
let () = Driver.register_transformation ~rules:[ rule ] "autoex"
