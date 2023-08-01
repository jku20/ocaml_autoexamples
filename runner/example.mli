module Example : sig
  type t = {
    function_name : string;
    example_body : string;
    loc : Ppxlib.location;
  }

  (** makes an `Example.t` taking a function name, example body and
      location. It is a nice convience function *)
  val make : string -> string -> Ppxlib.location -> t
end
