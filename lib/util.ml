let rec string_of_chars_aux count result char = if count <= 0 then result else string_of_chars_aux (count - 1) (Printf.sprintf "%c%s" char result) (char)
let string_of_chars count = string_of_chars_aux count ""

let string_of_module_path path = if path = "" then "" else Printf.sprintf "%s::" path

let are_same_lenght l1 l2 = ( = ) 0 (List.compare_lengths l1 l2)

let are_diff_lenght l1 l2 = not (are_same_lenght l1 l2)
module Occurence = struct
  type ('a) occurence = Empty | One of 'a | Multiple of 'a list

  exception Too_Many_Occurence

  let one = function
  | Empty -> raise Not_found
  | Multiple _ -> raise Too_Many_Occurence
  | One f -> f 

  let find_occurence predicate list = match list |> List.find_all predicate with [] -> Empty | t::[] -> One t | t::q -> Multiple (t::q)
end

module ListHelper = struct
  
end