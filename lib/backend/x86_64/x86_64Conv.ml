open X86_64Core
open X86_64Core.Register
open X86_64Core.Operande
open X86_64Core.Instruction
open KosuIrTAC.Asttac
open Util

let sizeofn = KosuIrTyped.Asttyconvert.Sizeof.sizeof


let translate_tac_expression ~str_lit_map ?(target_dst = (`Register {size = D; reg = R10} : dst))
  rprogram (fd: FrameManager.frame_desc) = function
  | { tac_expression = TEString s; expr_rktype = _ } ->
    let (SLit str_labl) = Hashtbl.find str_lit_map s in
    target_dst, load_label str_labl target_dst
| { tac_expression = TEFalse | TEmpty; expr_rktype = _ } ->
  begin match target_dst with
  | (`Register reg ) as rreg -> target_dst, [
    Instruction (Xor {size = reg.size; source = rreg; destination = rreg })
  ]
  | (`Address _) as addr -> target_dst, [
    Instruction (Mov {size = B; destination = addr; source = `ILitteral 0L})
  ]
end
| { tac_expression = TENullptr; expr_rktype = _ } ->
  begin match target_dst with
   | (`Register reg ) as rreg -> target_dst, [
    Instruction (Xor {size = reg.size; source = rreg; destination = rreg })
  ]
  | (`Address _) as addr -> target_dst, [
    Instruction (Mov {size = Q; destination = addr; source = `ILitteral 0L})
  ]
end
| { tac_expression = TETrue; _ } -> 
  target_dst, [ Instruction ( Mov {size = B; destination = target_dst; source = `ILitteral 1L}) ]
| { tac_expression = TEInt (_, isize, int64); _ } ->
    let size = data_size_of_isize isize in
    target_dst, [Instruction (Mov {size; destination = target_dst; source = `ILitteral int64})]
| { tac_expression = TEFloat _float; _ } ->
  failwith "X86_64: Mov Float todo"
| { tac_expression = TEIdentifier id; expr_rktype } ->
    let adress =
      FrameManager.address_of (id, expr_rktype) fd |> fun adr ->
      match adr with
      | Some a -> a
      | None -> failwith "X86_64: tte identifier setup null address"
    in
    let sizeof = sizeofn rprogram expr_rktype in
    let data_size = Option.value ~default:Q @@ data_size_of_int64 sizeof in
    begin match is_register_size sizeof with
    true -> ( 
      match target_dst with
      | `Register _ as reg -> target_dst, [ Instruction (Mov {size = data_size; destination = reg; source = `Address adress})]
      | `Address addr -> target_dst, [
        Instruction (Mov {size = data_size; destination = `Register raxq; source = `Address addr});
        Instruction (Mov {size = data_size; destination = target_dst; source = `Register raxq})
      ]
    )
    | false -> (
      match target_dst with
      | `Register reg -> 
        `Register (resize_register Q reg ), [
          Instruction (Lea {size = Q; source = adress; destination = reg})
        ]
      | `Address _ ->
        `Register (raxq), [
          Line_Com (Comment "Mov identifier larger than reg");
          Instruction (Lea {size = Q; source = adress; destination = raxq})
        ] 
    )
    end
 
| { tac_expression = TESizeof kt; _ } ->
    let sizeof = sizeofn rprogram kt in
    target_dst, [
      Line_Com (Comment "Sizeof ");
      Instruction (Mov {size = Q; destination = target_dst; source = `ILitteral sizeof})
    ]
| {
    tac_expression = TEConst { name; module_path };
    expr_rktype = RTString_lit;
  } ->   
    target_dst, load_label ~module_path name target_dst
| {
    tac_expression = TEConst { name; module_path };
    expr_rktype = RTInteger (_, size);
  } ->
    let data_size = data_size_of_isize size in
    let const_decl = 
      match rprogram |> KosuIrTyped.Asttyhelper.RProgram.find_const_decl ~name ~module_path with
      | None -> failwith (Printf.sprintf "No const decl for %s::%s" module_path name)
      | Some s -> s in
    let int_value =
      match const_decl.value.rexpression with
      | REInteger (_, _, value) -> value
      | _ -> failwith "Not an Integer" in
    
    resize_dst data_size target_dst, [Instruction (Mov {size = data_size; source = `ILitteral int_value; destination = target_dst})]
| _ -> failwith "X86_64 : Other expression"