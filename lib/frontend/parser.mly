%{
    open Ast
    open Position
%}


%token <Ast.signedness * Ast.isize * int64> Integer_lit
%token <string> String_lit
%token <float> Float_lit
%token <string> IDENT
%token <string> BUILTIN
%token <string> Constant
%token <string> Module_IDENT 
%token LPARENT RPARENT LBRACE RBRACE LSQBRACE RSQBRACE WILDCARD
%token SEMICOLON ARROWFUNC MINUSUP
%token ENUM EXTERNAL SIG FUNCTION STRUCT TRUE FALSE EMPTY SWITCH IF ELSE FOR CONST VAR OF CASES DISCARD NULLPTR SYSCALL OPERATOR
%token TRIPLEDOT
%token COMMA
%token PIPESUP
%token EQUAL
%token PIPE
%token OR
%token AND
%token XOR
%token AMPERSAND
%token DOUBLEQUAL DIF
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
%left OR
%left AND
%left PIPE
%left XOR
%left AMPERSAND
%left DOUBLEQUAL DIF
%left SUP SUPEQ INF INFEQ
%left SHIFTLEFT SHIFTRIGHT
%left PLUS MINUS
%left MULT DIV MOD
%nonassoc UMINUS NOT
%left MINUSUP
// %nonassoc ENUM EXTERNAL SIG FUNCTION STRUCT TRUE FALSE EMPTY SWITCH IF ELSE FOR CONST VAR

%start modul

%type <Ast.kbody> kbody
%type <Ast.ktype> ktype
%type <Ast._module> modul
%type <Ast.kexpression> expr

%%

modul:
    | pns = list(module_nodes) EOF { Mod pns }
;;

%inline located(X): x=X {
  Position.located_value $startpos $endpos x
};;

module_nodes:
    | enum_decl { NEnum $1 }
    | struct_decl { NStruct $1 }
    | external_func_decl { NExternFunc $1 }
    | operator_decl { NOperator $1 }
    | syscall_decl { NSyscall $1 }
    | function_decl { NFunction $1 }
    | sig_decl { NSigFun $1 }
    | const_decl { NConst $1 }
;;

enum_decl:
    | ENUM generics_opt=option( LPARENT l=separated_nonempty_list(COMMA, located(IDENT)) RPARENT { l }) LBRACE 
    variants=separated_list(COMMA,enum_assoc) 
    RBRACE name=located(IDENT) SEMICOLON { 
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
    | STRUCT generics_opt=option( LPARENT l=separated_nonempty_list(COMMA, located(IDENT) ) RPARENT { l }) LBRACE
    fields=separated_list(COMMA, id=located(IDENT) COLON kt=located(ktype) { id, kt  })
    RBRACE name=located(IDENT) SEMICOLON {
        {
            struct_name = name;
            generics = generics_opt |> Option.value ~default: [];
            fields;
        }
    }
;;

external_func_decl:
    | EXTERNAL id=located(IDENT) LPARENT ctypes=separated_list(COMMA, located(ctype)) varia=option( p=preceded(SEMICOLON, TRIPLEDOT { () }) { p } ) 
    RPARENT r_type=located(ctype) c_name=option(EQUAL s=String_lit { s }) SEMICOLON {
        {
            sig_name = id;
            fn_parameters = ctypes;
            is_variadic = varia |> Option.is_some;
            r_type;
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
    | SUP { Sup }
    | INF { Inf }
;;

operator_decl:
    | OPERATOR op=located(binary_operator_symbol) fields=delimited(LPARENT, id1=located(IDENT) COLON kt1=located(ktype) COMMA id2=located(IDENT) COLON kt2=located(ktype) { (id1,kt1), (id2, kt2) } , RPARENT) return_type=located(ktype) kbody=kbody {
        Binary {
            op;
            fields;
            return_type;
            kbody
        }
    }
    | OPERATOR op=delimited(LPARENT, located(unary_operator_symbol), RPARENT) field=delimited(LPARENT, id=located(IDENT) COLON kt=located(ktype) { id, kt} ,RPARENT) return_type=located(ktype) kbody=kbody {
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

kbody:
    | delimited(LBRACE, l=list(located(statement)) DOLLAR e=located(expr) { l , e } , RBRACE)  { $1 }
statement:
    | declarer located(IDENT) explicit_type=option(COLON k=located(ktype) {k} ) EQUAL expression=located(expr) SEMICOLON { 
        SDeclaration { 
            is_const = $1;
            variable_name = $2;
            explicit_type; 
            expression
        }
    }
    | located(IDENT) EQUAL located(expr) SEMICOLON { SAffection ($1, $3 ) }
    | DISCARD located(expr) SEMICOLON { SDiscard ($2) }
;;

syscall_decl:
    | SYSCALL syscall_name=located(IDENT) parameters=delimited(LPARENT, separated_list(COMMA, ct=located(ctype) { ct  }), RPARENT ) return_type=located(ctype) 
       LBRACE SYSCALL LPARENT opcode=located(Integer_lit) RPARENT RBRACE {
        let _, _, value = opcode in
        {
            syscall_name;
            parameters;
            return_type;
            opcode = value
        }
    }

function_decl:
    | FUNCTION name=located(IDENT) generics_opt=option(d=delimited(INF, separated_nonempty_list(COMMA, id=located(IDENT) {id}), SUP ) { d })
    parameters=delimited(LPARENT, separated_list(COMMA, id=located(IDENT) COLON kt=located(ktype) { id, kt  }), RPARENT )
    r_type=located(ktype) body=kbody {
        {
            fn_name = name;
            generics = generics_opt |> Option.value ~default: [];
            parameters;
            return_type = r_type;
            body
        }
    }
;;
sig_decl:
    | SIG name=IDENT generics_opt=option(d=delimited(INF, separated_nonempty_list(COMMA, id=IDENT {id}), SUP ) { d })  
    parameters=delimited(LPARENT, separated_list(COMMA, ktype ), RPARENT ) return_type=ktype SEMICOLON {
        {
            sig_name = name;
            generics = generics_opt |> Option.value ~default: [];
            parameters;
            return_type;
        }
    }
const_decl:
    | CONST Constant EQUAL located(Integer_lit) SEMICOLON {
        let sign, size, value = $4 in
        {
            const_name = $2;
            explicit_type = TInteger (sign, size);
            value = EInteger (sign, size, value);
        }
    }
    | CONST Constant EQUAL located(String_lit) SEMICOLON {
        {
            const_name = $2;
            explicit_type = TString_lit;
            value = EString $4
        }
    }
    | CONST Constant EQUAL located(Float_lit) SEMICOLON {
        {
            const_name = $2;
            explicit_type = TFloat;
            value = EFloat $4
        }
    }
either_color_equal:
    | COLON {}
    | EQUAL {}
expr:
    | Integer_lit { EInteger $1 }
    | String_lit { EString $1 }
    | Float_lit { EFloat $1 }
    | TRUE { True }
    | FALSE { False }
    | EMPTY { Empty }
    | NULLPTR { ENullptr }
    | SIZEOF delimited(LPARENT, preceded(COLON, located(expr)) , RPARENT) { ESizeof ( Either.Right( $2) ) }
    | SIZEOF delimited(LPARENT, t=located(ktype) { t } , RPARENT) { ESizeof (Either.Left $2)  }
    | nonempty_list(MULT) located(IDENT) { 
        EDeference ( $1 |> List.length , $2)
    }
    | AMPERSAND located(IDENT) { EAdress $2 }
    | expr PLUS expr { EBin_op (BAdd ($1, $3) ) }
    | expr MINUS expr { EBin_op (BMinus ($1, $3)) }
    | expr MULT expr { EBin_op (BMult ($1, $3)) }
    | expr DIV expr { EBin_op (BDiv ($1, $3)) }
    | expr MOD expr { EBin_op (BMod ($1, $3)) }
    | expr PIPE expr {  EBin_op (BBitwiseOr ($1, $3)) }
    | expr XOR expr {  EBin_op (BBitwiseXor ($1, $3)) }
    | expr AMPERSAND expr {  EBin_op (BBitwiseAnd ($1, $3)) }
    | expr SHIFTLEFT expr { EBin_op (BShiftLeft ($1, $3)) }
    | expr SHIFTRIGHT expr { EBin_op (BShiftRight ($1, $3)) }
    | expr AND expr { EBin_op (BAnd ($1, $3)) }
    | expr OR expr { EBin_op (BOr ($1, $3)) }
    | expr SUP expr { EBin_op (BSup ($1, $3)) }
    | expr SUPEQ expr { EBin_op (BSupEq ($1, $3)) }
    | expr INF expr { EBin_op (BInf ($1, $3)) }
    | expr INFEQ expr { EBin_op (BInfEq ($1, $3)) }
    | expr DOUBLEQUAL expr { EBin_op (BEqual ($1, $3)) }
    | expr DIF expr { EBin_op (BDif ($1, $3)) }
    | located(expr) MINUSUP separated_nonempty_list(MINUSUP, IDENT) {
        EFieldAcces {
            first_expr = $1;
            fields = $3
        }
    }
    | NOT expr { EUn_op (UNot $2) }
    | MINUS expr %prec UMINUS { EUn_op (UMinus $2) }
    | BUILTIN parameters=delimited(LPARENT, separated_list(COMMA, expr) ,RPARENT) {
        EBuiltin_Function_call {
            fn_name = $1;
            parameters
        }
    }
    | l=separated_list(DOUBLECOLON, Module_IDENT) name=IDENT generics_resolver=option(DOUBLECOLON INF s=separated_nonempty_list(COMMA, ktype) SUP { s } ) LPARENT exprs=separated_list(COMMA, expr) RPARENT {
        EFunction_call { 
            modules_path = l |> String.concat "/";
            generics_resolver;
            fn_name = name;
            parameters = exprs;
        }
    }
    | l=separated_list(DOUBLECOLON, Module_IDENT) id=located(IDENT) {
        EIdentifier { 
            modules_path = l |> String.concat "/";
            identifier = id
        }

    }
    | l=separated_list(DOUBLECOLON, Module_IDENT) id=located(Constant) {
        EConst_Identifier {
            modules_path = l |> String.concat "/";
            identifier = id
        }
    }
    | expr PIPESUP calls=separated_nonempty_list(PIPESUP, 
        modules=separated_list(DOUBLECOLON, Module_IDENT) name=IDENT 
        generics_resolver=option(DOUBLECOLON INF s=separated_nonempty_list(COMMA, ktype) SUP { s } )
         LPARENT exprs=separated_list(COMMA, expr) RPARENT { name, generics_resolver, exprs, modules }) {
            calls |> List.fold_left (
                fun acc value  -> 
                    let fn_name, generics_resolver, parameters, modules_path = value in
                    EFunction_call { 
                        modules_path = modules_path |> String.concat "/";
                        generics_resolver;
                        fn_name;
                        parameters = acc::parameters;
                    }
                ) $1
        }
    | modules_path=separated_list(DOUBLECOLON, Module_IDENT)  struct_name=located(IDENT) fields=delimited(LBRACE, separated_list(COMMA, located(id=IDENT either_color_equal  expr=expr { id, expr } ) ) , RBRACE) {
        EStruct {
            modules_path = modules_path |> String.concat "/";
            struct_name;
            fields
        }
    }
    | modules_path=separated_list(DOUBLECOLON, Module_IDENT) enum_name=option(IDENT) DOT variant=IDENT assoc_exprs=option(delimited(LPARENT, separated_nonempty_list(COMMA, expr) ,RPARENT)) {
        EEnum {
            modules_path = modules_path |> String.concat "/";
            enum_name;
            variant;
            assoc_exprs = assoc_exprs |> Option.value ~default: []
        }
    }
    | CASES delimited(LBRACE, 
        s=nonempty_list(OF conds=expr ARROWFUNC body=kbody { conds, body } ) 
        ELSE else_case=kbody { s, else_case } , RBRACE) {
            let cases, else_case = $2 in
        ECases {
            cases;
            else_case
        }
    }
    | IF expr kbody ELSE kbody  {
        EIf (
            $2, 
            $3, 
            $5
            (* $4 |> Option.map (fun e -> (SExpression e)::[] ) *)
        )
    }
    | SWITCH delimited(LPARENT, expr, RPARENT) LBRACE nonempty_list(cases=separated_nonempty_list(COMMA, s_case) ARROWFUNC stmts=kbody { cases, stmts } ) 
        wildcard_case=option(WILDCARD ARROWFUNC d=kbody { d } ) RBRACE { 
        ESwitch {
            expression = $2;
            cases = $4;
            wildcard_case
        }
    }
    | d=delimited(LPARENT, separated_list(COMMA, expr), RPARENT) {
        match d with
        | [] -> Empty
        | t::[] -> t
        | tuple -> ETuple tuple
    }
;;
s_case:
    | DOT IDENT { SC_Enum_Identifier { variant = $2 } }
    | DOT IDENT delimited(LPARENT, separated_nonempty_list(COMMA, IDENT { Some $1 } | WILDCARD { None } ), RPARENT) {
        SC_Enum_Identifier_Assoc {
            variant = $2;
            assoc_ids = $3
        }
    }

ctype:
    | modules_path=separated_list(DOUBLECOLON, located(Module_IDENT) ) id=located(IDENT) { 
        id |> Position.map (
        function
        | "f64" -> TFloat
        | "unit" -> TUnit
        | "bool" -> TBool
        | "stringl" -> TString_lit
        | "anyptr" -> TPointer (TUnknow)
        | "s8" -> TInteger( Signed, I8)
        | "u8" -> TInteger( Unsigned, I8)
        | "s16" -> TInteger( Signed, I16)
        | "u16" -> TInteger( Unsigned, I16)
        | "s32" -> TInteger( Signed, I32)
        | "u32" -> TInteger( Unsigned, I32)
        | "s64" -> TInteger( Signed, I64)
        | "u64" -> TInteger( Unsigned, I64)
        | _ as s -> TType_Identifier {
            module_path = modules_path |> Position.map( String.concat "/" ) ;
            name = s
        }
        )

     }
    | MULT located(ktype) { TPointer $2 } 

ktype:
    | modules_path=separated_list(DOUBLECOLON, located(Module_IDENT)) id=located(IDENT) { 
        id |> Position.map (
        function
        | "f64" -> TFloat
        | "unit" -> TUnit
        | "bool" -> TBool
        | "s8" -> TInteger( Signed, I8)
        | "u8" -> TInteger( Unsigned, I8)
        | "s16" -> TInteger( Signed, I16)
        | "u16" -> TInteger( Unsigned, I16)
        | "s32" -> TInteger( Signed, I32)
        | "u32" -> TInteger( Unsigned, I32)
        | "s64" -> TInteger( Signed, I64)
        | "u64" -> TInteger( Unsigned, I64)
        | "stringl" -> TString_lit
        | _ as s -> TType_Identifier {
            module_path = modules_path |> Position.map( String.concat "/" ) ;
            name = s
        } 
        )
     }
    | MULT located(ktype) { TPointer $2 }
    | LPARENT l=separated_nonempty_list(COMMA, located(ktype)) RPARENT 
    modules_path=separated_list(DOUBLECOLON, located(Module_IDENT)) id=located(IDENT) { 
        TParametric_identifier {
            module_path = modules_path |> Position.map( String.concat "/" ) ;
            parametrics_type = l;
            name = id
        }
    }
    | LPARENT l=separated_nonempty_list(COMMA, located(ktype) ) RPARENT { TTuple (l)  }
;;
