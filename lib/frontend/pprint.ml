open Lexer;;

let () = Printexc.register_printer (function
| Astvalidation.Error.Validation_error e -> e |> Astvalidation.Error.string_of_validation_error |> Option.some 
| _ -> None
)