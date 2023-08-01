open Ppxlib

(** give a `Ppxlib.loc` creates an expression which
    is if it were written literally *)
let make_location_literal loc =
  let {
    loc_start =
      {
        pos_fname = start_fname;
        pos_lnum = start_lnum;
        pos_bol = start_bol;
        pos_cnum = start_cnum;
      };
    loc_end =
      {
        pos_fname = end_fname;
        pos_lnum = end_lnum;
        pos_bol = end_bol;
        pos_cnum = end_cnum;
      };
    loc_ghost;
  } =
    loc
  in
  let (module B) = Ast_builder.make loc in
  let open B in
  [%expr
    {
      loc_start =
        {
          pos_fname = [%e estring start_fname];
          pos_lnum = [%e eint start_lnum];
          pos_bol = [%e eint start_bol];
          pos_cnum = [%e eint start_cnum];
        };
      loc_end =
        {
          pos_fname = [%e estring end_fname];
          pos_lnum = [%e eint end_lnum];
          pos_bol = [%e eint end_bol];
          pos_cnum = [%e eint end_cnum];
        };
      loc_ghost = [%e ebool loc_ghost];
    }]

let expand ~ctxt flg attributes fun_name expr =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let filename =
    Filename.basename (Expansion_context.Extension.input_name ctxt)
  in
  let (module B) = Ast_builder.make loc in
  let open B in
  let generator_name, to_string_name =
    let res =
      List.find_map
        (function
          | {
              attr_name = { txt = "autoex"; _ };
              attr_payload = Ppxlib.PStr [%str [%e? p1], [%e? p2]];
              attr_loc = _;
            } ->
              Some (p1, p2)
          | _ -> None)
        attributes
    in
    let error =
      pexp_extension
      @@ Location.error_extensionf ~loc "generator and/or stringifyer not found"
    in
    match res with
    | Some (x, y) -> ([%expr [%e x] ()], y)
    | None -> (error, error)
  in
  let location_literal = make_location_literal loc in
  let examples =
    [%expr
      [
        Runner.Example.Example.make [%e estring fun_name]
          (Runner.Collector.output_of
             (fun () -> [%e expr] [%e generator_name])
             [%e to_string_name])
          [%e location_literal];
      ]]
  in
  let write =
    [%expr
      match
        Runner.Writer.write_corrected_file [%e estring filename] [%e examples]
      with
      | Ok () -> ()
      | Error s -> failwith s]
  in
  pstr_value flg
    (value_binding ~pat:(pvar fun_name) ~expr
    :: [ value_binding ~pat:punit ~expr:write ])

let extension =
  let pat =
    Ast_pattern.(
      pstr
        (pstr_value __
           (value_binding_attributes __
              (value_binding ~pat:(ppat_var __) ~expr:__)
           ^:: nil)
        ^:: nil))
  in
  Extension.V3.declare "autoex" Extension.Context.structure_item pat expand

let rule = Ppxlib.Context_free.Rule.extension extension
let () = Driver.register_transformation ~rules:[ rule ] "autoex"
