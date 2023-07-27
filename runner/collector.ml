let output_of f =
  let open Unix in
  let old_stderr = dup stderr in
  let old_stdout = dup stdout in
  let tmp_name, tmp_channel = Filename.open_temp_file "foutput" "" in
  let tmp_descr = descr_of_out_channel tmp_channel in
  let result = ref "" in
  Fun.protect
    ~finally:(fun () ->
      dup2 old_stderr stderr;
      dup2 old_stdout stdout;
      close old_stdout;
      close old_stderr;
      close tmp_descr;
      (result := In_channel.(with_open_bin tmp_name input_all));
      Sys.remove tmp_name)
    (fun () ->
      dup2 tmp_descr stdout;
      dup2 tmp_descr stderr;
      f ());
  !result
