val output_of : (unit -> 'a) -> ('a -> string) -> string
(** captures `f`'s output to stderr and stdout and returns
   it as a string *)
