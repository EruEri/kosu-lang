(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of Kosu                                                                  *)
(* Copyright (C) 2022 Yves Ndiaye                                                             *)
(*                                                                                            *)
(* Kosu is free software: you can redistribute it and/or modify it under the terms            *)
(* of the GNU General Public License as published by the Free Software Foundation,            *)
(* either version 3 of the License, or (at your option) any later version.                    *)
(*                                                                                            *)
(* Kosu is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;          *)
(* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR           *)
(* PURPOSE.  See the GNU General Public License for more details.                             *)
(* You should have received a copy of the GNU General Public License along with Kosu.         *)
(* If not, see <http://www.gnu.org/licenses/>.                                                *)
(*                                                                                            *)
(**********************************************************************************************)


%{
    open Ast
    open Position
    (* menhir --list-errors lib/frontend/parser.mly > lib/frontend/parser.messages *)
    (* dune build @update_messsages *)

    let identifier_to_expr id_loc = 
        Position.map_use (fun id -> 
            EIdentifier {
                modules_path = {
                    v = String.empty;
                    position = Position.end_to_start id.position
                };
                identifier = id
            }
        ) id_loc 
%}


%token <(Ast.signedness * Ast.isize) option * int64> Integer_lit
%token <string> String_lit
%token <char> Char_lit
%token <Ast.fsize option * float> Float_lit
%token <string> IDENT
%token <string> BUILTIN
%token <string> Constant
%token <string> Module_IDENT 
%token LPARENT RPARENT LBRACE RBRACE LSQBRACE RSQBRACE WILDCARD
%token CROISILLION
%token SEMICOLON ARROWFUNC
%token TYPE OPAQUE
%token ENUM ARRAY EXTERNAL FUNCTION STRUCT TRUE FALSE EMPTY SWITCH IF ELSE CONST VAR OF CASES DISCARD NULLPTR SYSCALL OPERATOR WHILE
%token CMP_LESS CMP_EQUAL CMP_GREATER MATCH ADDRESSOF
%token TRIPLEDOT
%token COMMA
%token PIPESUP
%token EQUAL
%token PIPE
%token OR FULLOR
%token AND FULLAND
%token XOR
%token AMPERSAND
%token DOUBLEQUAL DIF INF_EQ_SUP
%token SUP SUPEQ INF INFEQ
%token SHIFTLEFT SHIFTRIGHT
%token PLUS MINUS
%token MULT DIV MOD
%token NOT SIZEOF DOLLAR
%token DOT
%token DOUBLECOLON COLON
%token EOF


// %left AMPERSAND XOR PIPE AND OR COMMA PIPESUP 
// %left DOUBLECOLON DOT MULT DIV MOD SHIFTLEFT SHIFTRIGHT SUP SUPEQ INF INFEQ DOUBLEQUAL DIF PLUS MINUS
// %nonassoc EOF LPARENT RPARENT LBRACE RBRACE SEMICOLON COLON ARROWFUNC TRIPLEDOT EQUAL Integer_lit String_lit
// %left COMMA
%left PIPESUP
%left OR FULLOR
%left AND FULLAND
%left PIPE
%left XOR
%left AMPERSAND
%left DOUBLEQUAL DIF INF_EQ_SUP
%left SUP SUPEQ INF INFEQ
%left SHIFTLEFT SHIFTRIGHT
%left PLUS MINUS
%left MULT DIV MOD
%nonassoc UMINUS NOT
%left DOT LSQBRACE
// %nonassoc ENUM EXTERNAL SIG FUNCTION STRUCT TRUE FALSE EMPTY SWITCH IF ELSE FOR CONST VAR

%start modul
%start iexpression_node

%type <Ast.kbody> kbody
%type <Ast.ktype> ktype
%type <Ast._module> modul
%type <Ast.kexpression> expr
%type <Ast.iexpression_node option> iexpression_node

%%

modul:
    | pns = list(module_nodes) EOF { Mod pns }
;;

iexpression_node:
    | iexpression_nodes { Some $1 }
    | EOF { None }

iexpression_nodes:
    | module_nodes { IModule_Node $1 }
    | statement { IStatement $1 }
    | DOLLAR located(expr) SEMICOLON { IExpression $2 }


%inline located(X): x=X {
  Position.located_value $startpos $endpos x
};;

%inline trailing_separated_list(sep, elt):
    | nonempty_list(terminated(elt, sep)) { $1 }
    | separated_nonempty_list(sep, elt) { $1 }


%inline module_path:
    | mp=located( loption(terminated(separated_nonempty_list(DOUBLECOLON, Module_IDENT), DOT)) ) { mp |> Position.map( String.concat "::") }
    // | mp=located( terminated(separated_list(DOUBLECOLON, Module_IDENT), DOT)) { mp |> Position.map( String.concat "::") }

%inline typed_parameter_loc(X):
    | WILDCARD COLON x=located(X) { x }
    | IDENT COLON x=located(X)  { x }
    // | x=located(X) { x }

%inline affected_value:
    | separated_nonempty_list(DOT, located(IDENT)) {
        match $1 with
        | [] -> failwith "unreachable"
        | t::[] -> AFVariable t
        | t::q -> AFField { variable = t; fields = q}
    }

module_nodes:
    | enum_decl { NEnum $1 }
    | struct_decl { NStruct $1 }
    | external_func_decl { NExternFunc $1 }
    | operator_decl { NOperator $1 }
    | syscall_decl { NSyscall $1 }
    | function_decl { NFunction $1 }
    | const_decl { NConst $1 }
    | opaque_decl { NOpaque $1 }
;;

pattern:
    | TRUE { PTrue }
    | FALSE { PFalse }
    | EMPTY { PEmpty }
    | CMP_LESS { PCmpLess }
    | CMP_EQUAL { PCmpEqual }
    | CMP_GREATER { PCmpGreater }
    | NULLPTR { PNullptr }
    | WILDCARD { PWildcard }
    | located(Float_lit) { 
        let value = Position.map snd $1 in
        PFloat value
    }
    | located(Char_lit) { 
        PChar $1
    }
    | option(MINUS) located(Integer_lit) {
        let is_neg = Option.is_some $1 in
        let value = Position.map snd $2 in
        let value = Position.map (fun value -> 
            match is_neg with
            | true -> Int64.neg value
            | false -> value
        ) value
        in
        PInteger value
    }
    | located(IDENT) {
        PIdentifier $1
    }
    | delimited(LPARENT, separated_list(COMMA, located(pattern)) ,RPARENT) {
        match $1 with
        | [] -> PEmpty
        | p::[] -> p.v
        | list -> PTuple list
    }
    | DOT located(IDENT) loption(delimited(LPARENT, separated_nonempty_list(COMMA, located(pattern)) ,RPARENT))  {
        PCase {
            variant = $2;
            assoc_patterns = $3
        }
    }
    | lpattern=located(pattern) PIPE rpattern=located(pattern) {
        let lpattern = Asthelper.Pattern.flatten_por lpattern in
        let rpattern = Asthelper.Pattern.flatten_por rpattern in
        let patterns = lpattern @ rpattern in
        POr patterns
    }
opaque_decl:
    | OPAQUE TYPE located(IDENT) {
        $3
    }

enum_decl:
    | ENUM name=located(IDENT) generics_opt=option( delimited(LPARENT, separated_nonempty_list(COMMA, located(IDENT) ), RPARENT)) LBRACE 
    variants=separated_list(COMMA,enum_assoc) 
    RBRACE { 
        let generics = generics_opt |> Option.value ~default: [] in
        {
            enum_name = name;
            generics = generics;
            variants = variants;
        }
    }
;;

enum_assoc:
    | id=located(IDENT) opt=option(LPARENT l=separated_nonempty_list(COMMA, located(ktype) ) RPARENT {l} ) {
        (
            id,
            opt |> Option.value ~default: []
        )
    }
;;

struct_decl:
    | STRUCT name=located(IDENT) generics_opt=option( delimited(LPARENT, separated_nonempty_list(COMMA, located(IDENT) ), RPARENT)) LBRACE
    fields=separated_list(COMMA, id=located(IDENT) COLON kt=located(ktype) { id, kt  })
    RBRACE  {
        {
            struct_name = name;
            generics = generics_opt |> Option.value ~default: [];
            fields;
        }
    }
;;

external_func_decl:
    | EXTERNAL id=located(IDENT) LPARENT ctypes=separated_list(COMMA, typed_parameter_loc(ctype)) varia=option( p=preceded(SEMICOLON, TRIPLEDOT { () }) { p } ) 
    RPARENT r_type=located(option(ctype)) c_name=option(EQUAL s=String_lit { s }) option(SEMICOLON) {
        {
            sig_name = id;
            fn_parameters = ctypes;
            is_variadic = varia |> Option.is_some;
            r_type = r_type |> Position.map (Option.value ~default: TUnit);
            c_name;
        }
    }
;;

unary_operator_symbol:
    | DOT NOT { PNot }
    | DOT MINUS { PUMinus }
;;

binary_operator_symbol:
    | PLUS { Add }
    | MINUS { Minus }
    | MULT { Mult }
    | DIV { Div }
    | MOD { Modulo }
    | AMPERSAND { BitwiseAnd }
    | PIPE { BitwiseOr }
    | XOR { BitwiseXor }
    | SHIFTLEFT { ShiftLeft }
    | SHIFTRIGHT { ShiftRight }
    | DOUBLEQUAL { Equal }
    | INF_EQ_SUP { Spaceship }
;;

operator_decl:
    | OPERATOR op=located(binary_operator_symbol) fields=delimited(LPARENT, id1=located(IDENT) COLON kt1=located(ktype) COMMA id2=located(IDENT) COLON kt2=located(ktype) { (id1,kt1), (id2, kt2) } , RPARENT) return_type=located(ktype) kbody=fun_kbody {
        Binary {
            op;
            fields;
            return_type;
            kbody
        }
    }
    | OPERATOR op=delimited(LPARENT, located(unary_operator_symbol), RPARENT) field=delimited(LPARENT, id=located(IDENT) COLON kt=located(ktype) { id, kt} ,RPARENT) return_type=located(ktype) kbody=fun_kbody {
        Unary {
            op;
            field;
            return_type;
            kbody
        }
    }
;;

declarer:
    | CONST { true }
    | VAR { false }
;;

function_call:
    modules_path=module_path
        fn_name=located(IDENT) 
        generics_resolver=option(DOUBLECOLON INF s=separated_nonempty_list(COMMA, located(ktype)) SUP { s } ) 
        LPARENT exprs=separated_list(COMMA, located(expr) ) RPARENT {
            modules_path, fn_name, generics_resolver, exprs
        }

fun_kbody:
    | EQUAL located(expr) option(SEMICOLON) { [], $2 }
    | fkbody { $1 }

%inline else_block_opt:
    | located(option(preceded(ELSE, kbody))) { 
        match $1.v with
        | Some body -> body
        | None -> [], ($1 |> Position.map (fun _ -> Empty))  
    }
    
%inline fkbody:
    | delimited(LBRACE, l=list(located(statement)) e=located(option(preceded(DOLLAR, located(expr)))) { l , e } , RBRACE)  { 
       let stmts, expr_loc = $1 in
       match expr_loc.v with
       | Some expr -> (stmts, expr)
       | None -> stmts, (expr_loc |> Position.map (fun _ -> Empty))
    }

kbody:
    | fkbody { $1 }
    | delimited(LPARENT, located(expr), RPARENT) { [], $1 }

statement:
    | declarer located(IDENT) explicit_type=option(COLON k=located(ktype) {k} ) EQUAL expression=located(expr) SEMICOLON { 
        SDeclaration { 
            is_const = $1;
            variable_name = $2;
            explicit_type; 
            expression
        }
    }
    | affected_value EQUAL located(expr) SEMICOLON { SAffection ($1, $3 ) }
    | preceded(MULT, affected_value) EQUAL located(expr) SEMICOLON { SDerefAffectation($1, $3)}
    | DISCARD located(expr) SEMICOLON { SDiscard ($2) }
;;

syscall_decl:
    | SYSCALL syscall_name=located(IDENT) parameters=delimited(LPARENT, separated_list(COMMA, typed_parameter_loc(ctype)), RPARENT ) return_type=located( option(ctype) ) EQUAL opcode=located(Integer_lit) option(SEMICOLON)
    {
        {
            syscall_name;
            parameters;
            return_type = return_type |> Position.map (Option.value ~default: TUnit);
            opcode = opcode |> Position.map (fun (_, value) -> value )
        }
    }

function_decl:
    | FUNCTION name=located(IDENT) generics_opt=option(d=delimited(INF, separated_nonempty_list(COMMA, id=located(IDENT) {id}), SUP ) { d })
    parameters=delimited(LPARENT, separated_list(COMMA, id=located(IDENT) COLON kt=located(ktype) { id, kt  }), RPARENT )
    r_type=located( option(ktype) ) body=fun_kbody {
        {
            fn_name = name;
            generics = generics_opt |> Option.value ~default: [];
            parameters;
            return_type = r_type |> Position.map (Option.value ~default: TUnit);
            body
        }
    }
;;
const_decl:
    | CONST located(Constant) EQUAL located(Integer_lit) option(SEMICOLON) {
        let sign, size = match fst $4.v with 
            | Some s -> s
            | None -> Ast.Type.default_integer_info 
        in
        {
            const_name = $2;
            explicit_type = TInteger (Some (sign, size));
            value = $4 |> Position.map (fun (_, value) -> EInteger (Some (sign, size), value) ) ;
        }
    }
    | CONST located(Constant) EQUAL located(String_lit) option(SEMICOLON) {
        {
            const_name = $2;
            explicit_type = TString_lit;
            value = $4 |> Position.map (fun s -> EString s)
        }
    }
    | CONST located(Constant) EQUAL located(Float_lit) option(SEMICOLON) {
        let fsize = match fst $4.v with
            | Some s -> s
            | None -> Ast.Type.default_float_info
        in
        {
            const_name = $2;
            explicit_type = TFloat (Some fsize); (* For the time, waiting struct constant refacto *)
            value = $4 |> Position.map ( fun (_, value) -> EFloat (Some (fsize), value))
        }
    }
either_color_equal:
    | COLON {}
    | EQUAL {}
enum_resolver:
    | DOT { None }
    | terminated(located(IDENT), DOUBLECOLON) { Some $1 }
expr:
    | Integer_lit { 
        let ss, value = $1 in
        EInteger (ss, value)
    }
    | String_lit { EString $1 }
    | Float_lit { 
        let size, value = $1 in
        EFloat (size, value) 
    }
    | Char_lit  { EChar $1 }
    | TRUE { True }
    | FALSE { False }
    | EMPTY { Empty }
    | NULLPTR { ENullptr }
    | CMP_LESS { ECmpLess }
    | CMP_EQUAL { ECmpEqual }
    | CMP_GREATER { ECmpGreater }
    | SIZEOF delimited(LPARENT, preceded(COLON, located(expr)) , RPARENT) { ESizeof ( Either.Right( $2) ) }
    | SIZEOF delimited(LPARENT, t=located(ktype) { t } , RPARENT) { ESizeof (Either.Left $2)  }
    | ADDRESSOF delimited(LPARENT, affected_value ,RPARENT) {
        EAdressof $2
    }
    | nonempty_list(MULT) located(IDENT) { 
        EDeference ( $1 |> List.length , $2)
    }
    | AMPERSAND located(IDENT) { EAdress $2 }
    | WHILE delimited(LPARENT, located(expr), RPARENT) kbody {
        EWhile ($2, $3)
    }
    | located(expr) INF_EQ_SUP located(expr) { EBin_op( BCmp ($1, $3)) }
    | located(expr) PLUS located(expr) { EBin_op (BAdd ($1, $3) ) }
    | located(expr) MINUS located(expr) { EBin_op (BMinus ($1, $3)) }
    | located(expr) MULT located(expr) { EBin_op (BMult ($1, $3)) }
    | located(expr) DIV located(expr) { EBin_op (BDiv ($1, $3)) }
    | located(expr) MOD located(expr) { EBin_op (BMod ($1, $3)) }
    | located(expr) PIPE located(expr) { EBin_op (BBitwiseOr ($1, $3)) }
    | located(expr) XOR located(expr) {  EBin_op (BBitwiseXor ($1, $3)) }
    | located(expr) AMPERSAND located(expr) {  EBin_op (BBitwiseAnd ($1, $3)) }
    | located(expr) SHIFTLEFT located(expr) { EBin_op (BShiftLeft ($1, $3)) }
    | located(expr) SHIFTRIGHT located(expr) { EBin_op (BShiftRight ($1, $3)) }
    | located(expr) FULLAND located(expr) { EBin_op (BAnd ($1, $3)) }
    | located(expr) FULLOR located(expr) { 
        EBin_op (BOr ($1, $3))
    }
    | located(expr) AND located(expr) {
        let true_block = ([], $3) in
        let false_block = ([], $1 |> Position.map (fun _ -> False)) in
        EIf ($1, true_block, false_block)
    }
    | located(expr) OR located(expr) { 
        let true_block = ( [], $1 |> Position.map (fun _ -> True) ) in
        let else_block = ( [], $3) in 
        EIf ($1, true_block, else_block ) 
    }
    | located(expr) SUP located(expr) { EBin_op (BSup ($1, $3)) }
    | located(expr) SUPEQ located(expr) { EBin_op (BSupEq ($1, $3)) }
    | located(expr) INF located(expr) { EBin_op (BInf ($1, $3)) }
    | located(expr) INFEQ located(expr) { EBin_op (BInfEq ($1, $3)) }
    | located(expr) DOUBLEQUAL located(expr) { EBin_op (BEqual ($1, $3)) }
    | located(expr) DIF located(expr) { EBin_op (BDif ($1, $3)) }
    | located(expr) DOT located(Integer_lit) {
        let value = $3 |> Position.map (fun (_, value) -> value) in
        ETupleAccess {
            first_expr = $1;
            index = value
        } 
    }
    | located(expr) DOT located(IDENT) {
        EFieldAcces {
            first_expr = $1;
            field = $3
        }
    }
    | located(expr) delimited(LSQBRACE, located(expr), RSQBRACE) {
        EArrayAccess {
            array_expr = $1;
            index_expr = $2
        }
    }
    | NOT located(expr) { EUn_op (UNot $2) }
    | MINUS located(expr) %prec UMINUS { EUn_op (UMinus $2) }
    | located(BUILTIN) parameters=delimited(LPARENT, separated_list(COMMA, located(expr)) ,RPARENT) {
        EBuiltin_Function_call {
            fn_name = $1;
            parameters
        }
    }
    | function_call {
        let modules_path, fn_name, generics_resolver, exprs = $1 in
        EFunction_call {
            modules_path;
            generics_resolver;
            fn_name;
            parameters = exprs
        }
    }
    | l=module_path id=located(IDENT) {
        if l.v = String.empty then 
            EIdentifier { 
                modules_path = l;
                identifier = id
            }
        else 
            EEnum {
                modules_path = l;
                enum_name = None;
                variant = id;
                assoc_exprs = []
            } 

    }
    | l=module_path id=located(Constant) {
        EConst_Identifier {
            modules_path = l ;
            identifier = id
        }
    }
    | located(expr) PIPESUP function_call {
        let modules_path, fn_name, generics_resolver, exprs = $3 in
        EFunction_call {
            modules_path;
            generics_resolver;
            fn_name;
            parameters = $1::exprs
        }
    }
    | modules_path=module_path struct_name=located(IDENT) fields=delimited(LBRACE, 
        separated_list(COMMA, id=located(IDENT) expr=option(preceded(either_color_equal, located(expr))) 
        { 
            let expr = Option.value ~default:(identifier_to_expr id) expr in
            id, expr 
        }
        ) ,RBRACE) {
        EStruct {
            modules_path;
            struct_name;
            fields
        }
    }
    
    | modules_path=module_path enum_name=enum_resolver variant=located(IDENT) assoc_exprs=option(delimited(LPARENT, separated_nonempty_list(COMMA, located(expr)) ,RPARENT)) {
        EEnum {
            modules_path;
            enum_name;
            variant;
            assoc_exprs = assoc_exprs |> Option.value ~default: []
        }
    }
    | CASES delimited(LBRACE, 
        s=nonempty_list(OF conds=located(expr) ARROWFUNC body=kbody { conds, body } ) 
        else_case=else_block_opt { s, else_case }, RBRACE) {
            let cases, else_case = $2 in
        ECases {
            cases;
            else_case
        }
    }
    | IF delimited(LPARENT, located(expr), RPARENT) kbody else_block_opt {
        EIf (
            $2, 
            $3, 
            $4

        )
    }
    | SWITCH delimited(LPARENT, located(expr), RPARENT) LBRACE nonempty_list(cases=separated_nonempty_list(PIPE, s_case) ARROWFUNC stmts=kbody { cases, stmts } ) 
        wildcard_case=option(WILDCARD ARROWFUNC d=kbody { d } ) RBRACE { 
        ESwitch {
            expression = $2;
            cases = $4;
            wildcard_case
        }
    }
    | MATCH delimited(LPARENT, located(expr), RPARENT) delimited(
        LBRACE,
        preceded(option(PIPE),
            separated_list(PIPE, p=located(pattern) ARROWFUNC body=kbody {p, body})),
        RBRACE
    ){
        EMatch {
            expression = $2;
            patterns = $3
        }
    }
    | delimited(LSQBRACE, separated_nonempty_list(COMMA, located(expr)), RSQBRACE) {
       EArray $1
    }
    | delimited(LSQBRACE, size=located(Integer_lit) COLON init_expr=located(expr) { size, init_expr }, RSQBRACE) {
        let size, expr = $1 in
        let _, size = size.v in
        let exprs = List.init (Int64.to_int size) (fun _ -> expr) in
        EArray exprs
    }
    | d=delimited(LPARENT, separated_list(COMMA, located(expr)), RPARENT) {
        match d with
        | [] -> Empty
        | t::[] -> t.v
        | tuple -> ETuple tuple
    }
;;
s_case:
    | DOT located(IDENT) { SC_Enum_Identifier { variant = $2 } }
    | DOT located(IDENT) delimited(LPARENT, separated_nonempty_list(COMMA, located(IDENT) { Some $1 } | WILDCARD { None } ), RPARENT) {
        SC_Enum_Identifier_Assoc {
            variant = $2;
            assoc_ids = $3
        }
    }

ctype:
    | modules_path=module_path id=located(IDENT) { 
        let open Ast.Type in
        match modules_path.v = String.empty with
        | true -> begin match id.v with
            | "unit" -> TUnit
            | "bool" -> TBool
            | "stringl" -> TString_lit
            | "f64" -> kt_f64
            | "f32" -> kt_f32
            | "s8" -> kt_s8
            | "u8" -> kt_u8
            | "s16" -> kt_s16
            | "u16" -> kt_u16
            | "s32" -> kt_s32
            | "u32" -> kt_u32
            | "s64" -> kt_s64
            | "u64" -> kt_u64
            | "anyptr" -> TPointer ({ v = TUnknow; position = id.position })
            | _ -> TType_Identifier {
                module_path = modules_path ;
                name = id
            }
        end
        | false -> 
            TType_Identifier {
                module_path = modules_path ;
                name = id
            }
    }
    | module_path=module_path CROISILLION name=located(IDENT) {
        TOpaque {module_path; name}
    }
    | MULT located(ktype) { TPointer $2 } 

ktype:
    | modules_path=module_path id=located(IDENT) {
        let open Type in
        match modules_path.v = String.empty with
        | false -> TType_Identifier {
            module_path = modules_path;
            name = id
        } 
        | true -> begin match id.v with
            | "unit" -> TUnit
            | "bool" -> TBool
            | "char" -> TChar
            | "f64" -> kt_f64
            | "f32" -> kt_f32
            | "s8" -> kt_s8
            | "u8" -> kt_u8
            | "s16" -> kt_s16
            | "u16" -> kt_u16
            | "s32" -> kt_s32
            | "u32" -> kt_u32
            | "s64" -> kt_s64
            | "u64" -> kt_u64
            | "order" -> TOredered
            | "stringl" -> TString_lit
            | _ -> TType_Identifier {
                module_path = modules_path;
                name = id
            } 
        end
    }
    | module_path=module_path CROISILLION name=located(IDENT) {
        TOpaque {module_path; name}
    }
    | ARRAY delimited(LPARENT, size=located(Integer_lit) COLON ktype=located(ktype) {ktype, size}, RPARENT) {
        let ktype, size = $2 in
        let size = Position.map (fun (_, value) -> value) size in
        TArray {
            size;
            ktype
        }
    }
    | MULT located(ktype) { TPointer $2 }
    | modules_path=module_path id=located(IDENT) l=delimited(LPARENT, separated_nonempty_list(COMMA, located(ktype)), RPARENT )  {
        TParametric_identifier {
            module_path = modules_path;
            parametrics_type = l;
            name = id
        }
    }
    | LPARENT l=separated_nonempty_list(COMMA, located(ktype) ) RPARENT { 
        match l with
        | [] -> TUnit
        | t::[] -> t.v
        | l -> TTuple (l) 
    }
;;
