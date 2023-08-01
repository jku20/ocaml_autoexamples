module Example : sig
  type t = {
    function_name : string;
    example_body : string;
    loc : Ppxlib.location;
  }

  val make : string -> string -> Ppxlib.location -> t
  (** makes an `Example.t` taking a function name, example body and
      location. It is a nice convience function *)
end
