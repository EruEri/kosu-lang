%{
    open Ast
%}


%token <Ast.signedness * Ast.isize * int64> Integer_lit
%token <string> String_lit
%token <string> IDENT
%token LPARENT RPARENT LBRACE RBRACE
%token SEMICOLON COLON ARROWFUNC
%token ENUM EXTERNAL SIG FUNCTION STRUCT TRUE FALSE EMPTY SWITCH IF ELSE FOR CONST VAR
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
%token NOT
%token DOT
%token DOUBLECOLON
%token EOF


%left AMPERSAND XOR PIPE AND OR COMMA PIPESUP 
%left DOUBLECOLON DOT MULT DIV MOD SHIFTLEFT SHIFTRIGHT SUP SUPEQ INF INFEQ DOUBLEQUAL DIF PLUS MINUS
// %nonassoc EOF LPARENT RPARENT LBRACE RBRACE SEMICOLON COLON ARROWFUNC TRIPLEDOT EQUAL Integer_lit String_lit
%nonassoc UMINUS NOT
// %nonassoc ENUM EXTERNAL SIG FUNCTION STRUCT TRUE FALSE EMPTY SWITCH IF ELSE FOR CONST VAR

%start prog


%type <Ast.ktype> ktype
%type <Ast.program> prog
%type <Ast.kexpression> expr

%%

prog:
    | pns = list(prog_nodes) EOF { Prog pns }
;;

prog_nodes:
    | enum_decl { NEnum $1 }
    | struct_decl { NStruct $1 }
    | external_func_decl { NExternFunc $1 }
    | function_decl { NFunction $1 }
;;

enum_decl:
    | ENUM generics_opt=option( LPARENT l=separated_nonempty_list(COMMA, IDENT) RPARENT { l }) LBRACE 
    variants=separated_list(COMMA,enum_assoc) 
    RBRACE name=IDENT SEMICOLON { 
        let generics = generics_opt |> Option.value ~default: [] in
        {
            enum_name = name;
            generics = generics;
            variants = variants;
        }
    }
;;

enum_assoc:
    | id=IDENT opt=option(LPARENT l=separated_nonempty_list(COMMA, ktype) RPARENT {l} ) {
        (
            id,
            opt |> Option.value ~default: []
        )
    }
;;

struct_decl:
    | STRUCT generics_opt=option( LPARENT l=separated_nonempty_list(COMMA, IDENT) RPARENT { l }) LBRACE
    fields=separated_list(COMMA, id=IDENT COLON kt=ktype { id, kt  })
    RBRACE name=IDENT SEMICOLON {
        {
            struct_name = name;
            generics = generics_opt |> Option.value ~default: [];
            fields;
        }
    }
;;

external_func_decl:
    | EXTERNAL id=IDENT LPARENT ctypes=separated_list(COMMA, ctype) varia=option( p=preceded(SEMICOLON, TRIPLEDOT { () }) { p } ) 
    RPARENT r_type=ctype c_name=option(EQUAL s=String_lit { s }) SEMICOLON {
        {
            sig_name = id;
            fn_parameters = ctypes;
            is_variadic = varia |> Option.is_some;
            r_type;
            c_name;
        }
    }
;;

declarer:
    | CONST { true }
    | VAR { false }
;;
statement:
    | declarer IDENT EQUAL expr SEMICOLON { SDeclaration ($2, $4, $1) }
    | IDENT EQUAL expr SEMICOLON { SAffection ($1, $3) }
    | expr { SExpression $1 }
;;


function_decl:
    | FUNCTION name=IDENT generics_opt=option( d=delimited(SUP, separated_nonempty_list(COMMA, id=IDENT {id}), INF ) { d })
    parameters=delimited(LPARENT, separated_list(COMMA, id=IDENT COLON kt=ktype { id, kt  }), RPARENT )
    r_type=ktype LBRACE body=nonempty_list(statement) RBRACE {
        {
            fn_name = name;
            generics = generics_opt |> Option.value ~default: [];
            parameters;
            return_type = r_type;
            body
        }
    }
;;
expr:
    | Integer_lit { EInteger $1 }
    | String_lit { EString $1 }
    | TRUE { True }
    | FALSE { False }
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
    | expr PIPESUP calls=separated_nonempty_list(PIPESUP, function_call) {
        calls |> List.fold_left (fun acc (fn_name, exprs, module_resolve) -> 
            EFunction_call (
                (fn_name, acc::exprs),
                module_resolve
            )
        ) $1
    }
    | d=delimited(LPARENT, expr, RPARENT ) { d }
;;
function_call:
    | option(module_resolve) IDENT delimited(LPARENT, list(expr) ,RPARENT) {
        (
            $2, $3, $1
        )
    }
    ;;
module_resolve:
    | separated_list(DOUBLECOLON, IDENT) COLON { $1 }
ctype:
    | id=IDENT { 
        match id with
        | "float" -> TFloat
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
        | _ as s -> TType_Identifier s
     }
    | MULT ktype { TPointer $2 } 

ktype:
    | id=IDENT { 
        match id with
        | "float" -> TFloat
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
        | _ as s -> TType_Identifier s
     }
    | MULT ktype { TPointer $2 }
    | LPARENT l=separated_nonempty_list(COMMA, ktype) RPARENT id=IDENT { TParametric_identifier (id, l)  }
    | LPARENT l=separated_nonempty_list(COMMA, ktype) RPARENT { TTuple (l)  }
;;
