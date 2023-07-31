open Ppxlib

let expand ~ctxt flg fun_name fl fe fp attributes body =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let (module B) = Ast_builder.make loc in
  let open B in
  let expr = pexp_fun fl fe fp body in
  let generator_name =
    let res =
      List.find_map
        (function
          | {
              attr_name = { txt = "autoex_gen"; _ };
              attr_payload = Ppxlib.PStr [%str [%e? p]];
              attr_loc = _;
            } ->
              Some p
          | _ -> None)
        attributes
    in
    match res with Some x -> [%expr [%e x] ()] | None -> eunit
  in
  pstr_value flg
    (value_binding ~pat:(pvar fun_name) ~expr
    :: [ value_binding ~pat:punit ~expr:[%expr [%e expr] [%e generator_name]] ]
    )

let extension =
  let pat =
    Ast_pattern.(
      pstr
        (pstr_value __
           (value_binding ~pat:(ppat_var __)
              ~expr:(pexp_fun __ __ __ (pexp_attributes __ __))
           ^:: nil)
        ^:: nil))
  in
  Extension.V3.declare "autoex" Extension.Context.structure_item pat expand

let rule = Ppxlib.Context_free.Rule.extension extension
let () = Driver.register_transformation ~rules:[ rule ] "autoex"
