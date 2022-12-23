open Aarch64Core
open Aarch64Pprint
open Util


let export_asm_module {filename; asm_module_path; rprogram = _; str_lit_map} = 
  let file = open_out filename in
  let AsmModule rnodes = sort_asm_module asm_module_path.asm_module in
  let () = rnodes |> List.iter (fun node -> Printf.fprintf file "%s\n\n" (string_of_asm_node node)) in
  let () = Printf.fprintf file "\n\t.section	__TEXT,__cstring,cstring_literals\n" in 
  let () = str_lit_map |> Hashtbl.to_seq |> Seq.iter (fun (str, SLit label) -> Printf.fprintf file "%s:\n\t .asciz \"%s\"\n\n" label str) in 
  let () = Printf.fprintf file "\n.subsections_via_symbols\n" in
  let () = close_out file in
  filename

let export_asm_module_tmp {filename; asm_module_path; rprogram = _; str_lit_map} = 
  let filename = filename |> String.map (fun c -> if (Char.escaped c)  = Filename.dir_sep then '_' else c) in
  let filename, file = Filename.open_temp_file filename ".S" in
  let AsmModule rnodes = sort_asm_module asm_module_path.asm_module in
  let () = rnodes |> List.iter (fun node -> Printf.fprintf file "%s\n\n" (string_of_asm_node node)) in
  let () = Printf.fprintf file "\n\t.section	__TEXT,__cstring,cstring_literals\n" in 
  let () = str_lit_map |> Hashtbl.to_seq |> Seq.iter (fun (str, SLit label) -> Printf.fprintf file "%s:\n\t .asciz \"%s\"\n\n" label str) in 
  let () = Printf.fprintf file "\n.subsections_via_symbols\n" in
  let () = close_out file in
  filename

let compile_asm_tmp asm_program = 
  asm_program |> List.map (fun asm_module -> export_asm_module_tmp asm_module)

let compile_asm asm_program = 
  asm_program |> List.map (fun asm_module -> 
    export_asm_module asm_module
  )

let compile_asm_from_tac_tmp tac_program = 
  tac_program
  |> Aarch64Core.asm_program_of_tac_program
  |> compile_asm_tmp

let compile_asm_from_tac tac_program = 
  tac_program 
    |> Aarch64Core.asm_program_of_tac_program 
    |> compile_asm