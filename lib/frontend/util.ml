let rec string_of_chars_aux count result char =
  if count <= 0 then result
  else string_of_chars_aux (count - 1) (Printf.sprintf "%c%s" char result) char

let string_of_chars count = string_of_chars_aux count ""

let string_of_module_path path =
  if path = "" then "" else Printf.sprintf "%s::" path

let are_same_lenght l1 l2 = 0 = List.compare_lengths l1 l2
let are_diff_lenght l1 l2 = not (are_same_lenght l1 l2)

let dummy_generic_map generic_names parametrics_types =
  let list_len = parametrics_types |> List.length in
  let dummy_list = List.init list_len (fun _ -> ()) in
  let dummy_parametrics = List.combine dummy_list parametrics_types in
  let generics_mapped = List.combine generic_names dummy_parametrics in
  Hashtbl.of_seq (generics_mapped |> List.to_seq)

module Occurence = struct
  type 'a occurence = Empty | One of 'a | Multiple of 'a list

  exception Too_Many_Occurence
  exception No_Occurence

  let is_one = function One _ -> true | _ -> false

  let one = function
    | Empty -> raise No_Occurence
    | Multiple _ -> raise Too_Many_Occurence
    | One f -> f

  let find_occurence predicate list =
    match list |> List.find_all predicate with
    | [] -> Empty
    | [ t ] -> One t
    | t :: q -> Multiple (t :: q)
end

module ListHelper = struct
  let rec index_of_aux f index = function
    | [] -> raise Not_found
    | t :: q -> if f t then index else index_of_aux f (index + 1) q

  let index_of f = index_of_aux f 0
  let head_opt = function [] -> None | t :: _ -> Some t

  let rec duplicate_aux hashmap list =
    match list with
    | [] ->
        hashmap |> Hashtbl.to_seq |> List.of_seq
        |> List.filter_map (fun (key, value) ->
               if value > 1 then Some key else None)
    | t :: q ->
        let () =
          match Hashtbl.find_opt hashmap t with
          | Some value -> Hashtbl.replace hashmap t (value + 1)
          | None -> Hashtbl.add hashmap t 1
        in
        duplicate_aux hashmap q

  let duplicate l = duplicate_aux (Hashtbl.create (l |> List.length)) l
end
