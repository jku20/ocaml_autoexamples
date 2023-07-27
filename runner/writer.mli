open Example

val write_corrected_file : string -> Example.t list -> (unit, string) result
(** given a filename and a list of outputs from
    run examples, writes the corrected file with
    the name filename.ml.corrected *)
