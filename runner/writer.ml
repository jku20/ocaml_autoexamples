open Example

(* (?:\u{D A}|(?!\u{D A})[\u{A}-\u{D}\u{85}\u{2028}\u{2029}]
   see http://www.unicode.org/reports/tr18/#Line_Boundaries
*)
let newline_regex_string =
  "\\(\u{A}\\|\u{B}\\|\u{C}\\|\u{D}\\|\u{D}\u{A}\\|\u{85}\\|\u{2028}\\|\u{2029}\\)"

let newline_regex = Str.regexp newline_regex_string

(* TODO: make this not just assume the newline is \n *)
let os_newline = "\n"

(* TODO: figure out this indentation *)
let file_indent = "    "

(* TODO: figure out a better way to get rid of magic or not hardcode it *)
let magic_header = "-1-2-5-3-9-----EXAMPLE-----1-0-9-7-3-"
let magic_footer = "-1-2-5-3-9-----------------1-0-9-7-0-"

(* regex for finding useful pieces of text *)
let comment_close_regex_string = "*)"
let comment_open_regex_string = "(*"
let comment_close_regex = Str.regexp_string comment_close_regex_string
let comment_open_regex = Str.regexp_string comment_open_regex_string

let autoexample_regex =
  Str.regexp
    (magic_header ^ "\\(.\\|" ^ newline_regex_string ^ "\\)*" ^ magic_footer)

(** formats an example to be printed to as a comment *)
let format_example ({ function_name; example_body; _ } : Example.t) =
  let next_line = os_newline ^ file_indent in
  Printf.sprintf "%s%sfunction \"%s\" prints: %s%s%s" magic_header next_line
    function_name example_body next_line magic_footer

(** Given the contents of a file and an example
    presumably in the file, returns the file contents
    with the example corrected split like that *)
let fix_example ({ function_name; example_body; loc } : Example.t) file_contents
    =
  let ( let* ) = Result.bind in
  let fun_line_num = loc.loc_start.pos_lnum in
  let lines = Str.split newline_regex file_contents in
  let fix_comment_block_above contents line =
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
    let* pr, l, pf = loop (line - 1) (contents, "", []) in
    try
      ignore (Str.search_forward comment_close_regex l 0);
      (* two cases, either we have to find the previously done
         headers/footers and replace them or we have to make them
         ourself *)
      let rpr = List.rev pr in
      let rec loop = function
        | h :: t
          when try
                 ignore (Str.search_forward comment_open_regex h 0);
                 true
               with Not_found -> false ->
            (h, t)
        | h :: t ->
            let acc, rem = loop t in
            (acc ^ os_newline ^ h, rem)
        | [] ->
            raise
              (Failure
                 "SHOULD BE IMPOSSIBLE STATE, VERY BAD: close comment but no \
                  open comment")
      in
      let comment_string, rem = loop (l :: rpr) in
      let rem = List.rev rem in
      let formatted_example =
        format_example { function_name; example_body; loc }
      in
      let corrected_comment =
        if
          try
            ignore (Str.search_forward autoexample_regex comment_string 0);
            true
          with Not_found -> false
        then
          Str.substitute_first autoexample_regex
            (fun _ -> formatted_example)
            comment_string
        else
          Str.substitute_first comment_close_regex
            (fun _ ->
              os_newline ^ file_indent ^ formatted_example
              ^ comment_close_regex_string)
            comment_string
      in
      Ok (rem @ [ corrected_comment ] @ pf)
    with Not_found ->
      Printf.printf "Looking at line %s" l;
      Error
        (Printf.sprintf
           "Error: line %d does not have a comment in the line above it"
           fun_line_num)
  in
  let* lines = fix_comment_block_above lines fun_line_num in
  let* out =
    Ok (List.fold_left (fun acc v -> acc ^ os_newline ^ v) "" lines ^ os_newline)
  in
  Ok (String.sub out 1 (String.length out - 1))

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
