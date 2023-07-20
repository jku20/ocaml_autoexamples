module Example : sig
  type t = { function_name : string; example_body : string; loc: Ppxlib.location }
end
