open Asttac
open Printf
open KosuFrontend.Ast
open KosuIrTyped.Asttpprint

let symbole_of_unary unary =
  match unary.unop with TacNot -> "not" | TacUminus -> "(-.)"

let symbole_of_binary binary =
  match binary.binop with
  | TacSelf TacAdd -> "+"
  | TacSelf TacMinus -> "-"
  | TacSelf TacMult -> "*"
  | TacSelf TacDiv -> "/"
  | TacSelf TacModulo -> "%"
  | TacSelf TacBitwiseAnd -> "&"
  | TacSelf TacBitwiseOr -> "|"
  | TacSelf TacBitwiseXor -> "^"
  | TacSelf TacShiftLeft -> "<<"
  | TacSelf TacShiftRight -> ">>"
  | TacBool TacAnd -> "&&"
  | TacBool TacOr -> "||"
  | TacBool TacSup -> ">"
  | TacBool TacSupEq -> ">="
  | TacBool TacInf -> "<"
  | TacBool TacInfEq -> "<="
  | TacBool TacEqual -> "=="
  | TacBool TacDiff -> "!="

let rec string_of_label_tac_body ?(end_jmp = None) tac_body =
  sprintf "%s:\n\t%s \n\n" tac_body.label
    (string_of_tac_body ~end_jmp tac_body.body)

and string_of_tac_statement = function
  | STacDeclaration { identifier; expression } ->
      sprintf "%s = %s" identifier (string_of_tac_rvalue expression)
  | STacModification { identifier; expression } ->
      sprintf "%s <- %s" identifier (string_of_tac_rvalue expression)
  | STDerefAffectation { identifier; expression } ->
      sprintf "*%s <- %s" identifier (string_of_tac_rvalue expression)
  | STIf
      {
        statement_for_bool;
        condition_rvalue;
        if_tac_body;
        goto1;
        goto2;
        exit_label;
        else_tac_body;
      } ->
      let buffer = Buffer.create 64 in
      let () =
        statement_for_bool
        |> List.iter (fun stmt ->
               let () =
                 Buffer.add_string buffer (string_of_tac_statement stmt)
               in
               Buffer.add_char buffer '\n')
      in
      let () =
        Buffer.add_string buffer
          (sprintf "\tif %s goto %s\n"
             (string_of_tac_expression condition_rvalue)
             goto1)
      in
      let () = Buffer.add_string buffer (sprintf "\tjump %s\n" goto2) in

      let () =
        Buffer.add_string buffer
          (string_of_label_tac_body ~end_jmp:(Some exit_label) if_tac_body)
      in
      let () =
        Buffer.add_string buffer (string_of_label_tac_body else_tac_body)
      in
      let () = Buffer.add_string buffer (sprintf "\n%s" exit_label) in
      Buffer.contents buffer
  | SCases { cases; exit_label; else_tac_body } ->
      sprintf "%s\n%s%s:\n"
        (cases |> List.map string_of_tac_case |> String.concat "\n")
        (else_tac_body |> string_of_label_tac_body)
        exit_label
  | STSwitch
      {
        statemenets_for_case;
        condition_switch;
        sw_cases;
        wildcard_label;
        wildcard_body;
        sw_exit_label;
      } ->
      let buffer = Buffer.create 86 in
      let s_condition = string_of_tac_expression condition_switch in
      let () =
        statemenets_for_case
        |> List.iter (fun stmt ->
               let () =
                 Buffer.add_string buffer
                   (sprintf "%s\n\t" (string_of_tac_statement stmt))
               in
               ())
      in
      let () = Buffer.add_string buffer (sprintf "\n\t%s\n" s_condition) in
      let () =
        sw_cases
        |> List.iter (fun { variants_to_match; sw_goto; assoc_bound = _; _ } ->
               let () =
                 variants_to_match
                 |> List.iter (fun variant ->
                        let () =
                          Buffer.add_string buffer
                            (sprintf "\tif %s = %s goto %s" s_condition variant
                               sw_goto)
                        in
                        let () = Buffer.add_string buffer (sprintf "\n") in
                        ())
               in
               let () =
                 Buffer.add_string buffer
                   (wildcard_label
                   |> Option.map (sprintf "\n\tjump %s\n")
                   |> Option.value ~default:"")
               in
               ())
      in

      let () =
        sw_cases
        |> List.iter (fun { sw_exit_label; switch_tac_body; _ } ->
               let () =
                 Buffer.add_string buffer
                   (string_of_label_tac_body ~end_jmp:(Some sw_exit_label)
                      switch_tac_body)
               in
               ())
      in

      let () =
        wildcard_body
        |> Option.iter (fun body ->
               let s =
                 sprintf "%s"
                   (string_of_label_tac_body ~end_jmp:(Some sw_exit_label) body)
               in
               let () = Buffer.add_string buffer s in
               ())
      in

      let () = Buffer.add_string buffer (sprintf "%s" sw_exit_label) in
      Buffer.contents buffer

and string_of_tac_body ?(end_jmp = None) (statemements, expression) =
  sprintf "%s\n\t%s%s"
    (statemements |> List.map string_of_tac_statement |> String.concat "\n\t")
    (string_of_tac_expression expression)
    (end_jmp
    |> Option.map (fun s -> sprintf "\n\tjump %s" s)
    |> Option.value ~default:"")

and string_of_tac_expression = function
  | TEFalse -> "false"
  | TETrue -> "true"
  | TEmpty -> "empty"
  | TENullptr -> "nullptr"
  | TEInt (sign, _, value) ->
      let format = if sign = Unsigned then sprintf "%Lu" else sprintf "%Ld" in
      format value
  | TEFloat float -> string_of_float float
  | TEIdentifier id -> id
  | TEString s -> sprintf "\"%s\"" s
  | TEConst { module_path; name } -> sprintf "%s::%s" module_path name
  | TESizeof rktype -> sprintf "sizeof(%s)" (string_of_rktype rktype)

and string_of_tac_rvalue = function
  | RVUminus rvalue -> sprintf "uminus(%s)" (string_of_tac_rvalue rvalue)
  | RVNeg rvalue -> sprintf "!%s" (string_of_tac_rvalue rvalue)
  | RVExpression expr -> string_of_tac_expression expr
  | RVFunction { module_path; fn_name; generics_resolver; tac_parameters } ->
      sprintf "%s%s%s(%s)"
        (Util.string_of_module_path module_path)
        fn_name
        (generics_resolver
        |> Option.map (fun kts ->
               sprintf "::<%s>"
                 (kts |> List.map string_of_rktype |> String.concat ", "))
        |> Option.value ~default:"")
        (tac_parameters
        |> List.map string_of_tac_expression
        |> String.concat ", ")
  | RVStruct { module_path; struct_name; fields } ->
      sprintf "%s%s { %s }"
        (sprintf "%s::" module_path)
        struct_name
        (fields
        |> List.map (fun (id, expr) ->
               sprintf "%s: %s" id (string_of_tac_expression expr))
        |> String.concat ", ")
  | RVEnum { module_path; enum_name; variant; assoc_tac_exprs } ->
      sprintf "%s%s.%s%s"
        (Util.string_of_module_path module_path)
        (enum_name |> Option.fold ~none:" " ~some:Fun.id)
        variant
        (if assoc_tac_exprs = [] then ""
        else
          sprintf "(%s)"
            (assoc_tac_exprs
            |> List.map string_of_tac_expression
            |> String.concat ", "))
  | RVTuple exprs ->
      sprintf "(%s)"
        (exprs |> List.map string_of_tac_expression |> String.concat ", ")
  | RVFieldAcess { first_expr; field } ->
      sprintf "%s->%s" (string_of_tac_expression first_expr) field
  | RVAdress id -> sprintf "&%s" id
  | RVDefer id -> sprintf "*%s" id
  | RVUnop un -> string_of_tac_unary un
  | RVBinop bin -> string_of_tac_binary bin
  | RVBuiltinCall {fn_name; parameters} -> sprintf "@%s(%s)" fn_name (parameters |> List.map string_of_tac_expression |> String.concat ", ")
  | RVLater -> "lateinit"
  | RVDiscard -> "discard"

and string_of_tac_binary binary =
  let symbole = symbole_of_binary binary in
  let lhs = string_of_tac_expression binary.blhs in
  let rhs = string_of_tac_expression binary.brhs in
  sprintf "%s %s %s" lhs symbole rhs

and string_of_tac_unary unary =
  let symbole = symbole_of_unary unary in
  let lhs = string_of_tac_expression unary.expr in
  sprintf "%s %s" symbole lhs

and string_of_tac_case
    {
      condition_label;
      statement_for_condition;
      condition;
      goto;
      jmp_false;
      end_label;
      tac_body;
    } =
  sprintf "%s\n\t%s\n\tif %s goto %s\n%s\n%s"
    (condition_label |> Option.map (sprintf "%s:") |> Option.value ~default:"")
    (statement_for_condition
    |> List.map string_of_tac_statement
    |> String.concat "\n\t")
    (string_of_tac_expression condition)
    goto
    (jmp_false |> sprintf "\tjump %s")
    (string_of_label_tac_body ~end_jmp:(Some end_label) tac_body)
