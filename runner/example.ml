module Example = struct
  type t = {
    function_name : string;
    example_body : string;
    loc : Ppxlib.location;
  }

  let make function_name example_body loc = { function_name; example_body; loc }
end
