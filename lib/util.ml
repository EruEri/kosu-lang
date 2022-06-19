let rec string_of_chars_aux count result char = if count <= 0 then result else string_of_chars_aux (count - 1) (Printf.sprintf "%c%s" char result) (char)
let string_of_chars count char = string_of_chars_aux count "" char

let string_of_module_path path = if path = "" then "" else Printf.sprintf "%s::" path