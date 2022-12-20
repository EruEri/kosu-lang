open Aarch64Core
open Aarch64Pprint
open Util


let export_asm_module {filename; asm_module_path; rprogram = _; str_lit_map} = 
  let file = open_out filename in
  let AsmModule rnodes = sort_asm_module asm_module_path.asm_module in
  let () = rnodes |> List.iter (fun node -> Printf.fprintf file "%s\n\n" (string_of_asm_node node)) in
  let () = str_lit_map |> Hashtbl.to_seq |> Seq.iter (fun (str, SLit label) -> Printf.fprintf file "%s:\n\t .asciz \"%s\"\n\n" label str) in 
  let () = close_out file in
  ()


let compile_asm asm_program = 
  asm_program |> List.iter (fun asm_module -> 
    export_asm_module asm_module
  )