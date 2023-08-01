let gen () = "world"
let foo s = Printf.sprintf "hello %s" s
and () =
  match Runner.Writer.write_corrected_file "rewriter_tests.ml"
          [Runner.Example.Example.make "foo"
             (Runner.Collector.output_of
                (fun () -> (fun s -> Printf.sprintf "hello %s" s) (gen ()))
                Fun.id)
             {
               loc_start =
                 {
                   pos_fname = "rewriter_tests.ml";
                   pos_lnum = 4;
                   pos_bol = 34;
                   pos_cnum = 34
                 };
               loc_end =
                 {
                   pos_fname = "rewriter_tests.ml";
                   pos_lnum = 4;
                   pos_bol = 34;
                   pos_cnum = 103
                 };
               loc_ghost = true
             }]
  with
  | Ok () -> ()
  | Error s -> failwith s
