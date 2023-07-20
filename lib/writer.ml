open Example

(* (?:\u{D A}|(?!\u{D A})[\u{A}-\u{D}\u{85}\u{2028}\u{2029}]
   see http://www.unicode.org/reports/tr18/#Line_Boundaries
*)
let newline_regex =
  Str.regexp
    {|\(\u{A}\|\u{B}\|\u{C}\|\u{D}\|\u{D}\u{A}\|\u{85}\|\u{2028}\|\u{2029}\)|}

(* regex for closing an ocaml comment *)
let comment_close_regex = Str.regexp_string "*)"

(* TODO: make this not just assume the newline is \n *)
let os_newline = "\n"

(* TODO: figure out this indentation *)
let file_indent = "    "

(** formats an example to be printed to as a comment *)
let format_example ({ function_name; example_body; _ } : Example.t) =
  let prefix = os_newline ^ file_indent in
  let postfix = os_newline in
  Printf.sprintf "%sfunction \"%s\" prints: %s%s" prefix function_name
    example_body postfix

(** Given the contents of a file and an example
    presumably in the file, returns the file contents
    with the example corrected *)
let fix_example ({ function_name; example_body; loc } : Example.t) file_contents
    =
  let ( let* ) = Result.bind in
  let fun_line_num = loc.loc_start.pos_lnum in
  let lines = Str.split newline_regex file_contents in
  let patrition_on_modified_line contents line =
    let rec loop i = function
      | l :: t, _, _ when i = 1 -> Ok ([], l, t)
      | l :: t, a, b -> (
          match loop (i - 1) (t, a, b) with
          | Ok (first, second, third) -> Ok (l :: first, second, third)
          | Error _ as err -> err)
      | [], _, _ ->
          Error
            (Printf.sprintf
               "Error: lines %d does not exist, contents only has %d lines" line
               (List.length contents))
    in
    loop line (contents, "", [])
  in
  let* prefix, comment_line, postfix =
    if fun_line_num = 1 then
      Error "Error: line 1 does not have a comment in the line above it"
    else
      let* pr, l, pf = patrition_on_modified_line lines (fun_line_num - 1) in
      try
        ignore (Str.search_forward comment_close_regex l 0);
        Ok (pr, l, pf)
      with Not_found ->
        Error
          (Printf.sprintf
             "Error: line %d does not have a comment in the line above it"
             fun_line_num)
  in
  (* yeah this does the regex comment search twice but not that big a deal
      I just think it is nicer this way *)
  let comment_pre, comment_post =
    let splits =
      let open Str in
      let open List in
      full_split comment_close_regex comment_line
      |> map (fun v -> match v with Text s -> s | Delim s -> s)
      |> rev
    in
    match splits with
    | post :: delim :: pre ->
        let pre = List.fold_right (fun acc v -> acc ^ v) pre "" in
        (pre, delim ^ post)
    | _ ->
        raise
          (Failure
             "SHOULD BE IMPOSSIBLE STATE, VERY BAD: no comment found despite \
              check")
  in
  let formatted_example = format_example { function_name; example_body; loc } in
  let lines =
    prefix @ [ comment_pre ^ formatted_example ^ comment_post ] @ postfix
  in
  Ok (List.fold_left (fun acc v -> acc ^ os_newline ^ v) "" lines ^ os_newline)

let write_corrected_file file_name examples =
  let ic = open_in_bin file_name in
  let contents = In_channel.input_all ic in
  In_channel.close ic;
  let to_write =
    List.fold_left
      (fun acc v -> Result.bind acc (fix_example v))
      (Ok contents) examples
  in
  match to_write with
  | Error _ as err -> err
  | Ok s ->
      let oc = open_out_bin (file_name ^ ".corrected") in
      Out_channel.output_string oc s;
      Out_channel.close oc;
      Ok ()
