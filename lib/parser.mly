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
%token COLONDOUBLE
%token EOF


%left AMPERSAND XOR PIPE AND OR COMMA PIPESUP
%left COLONDOUBLE DOT MULT DIV MOD SHIFTLEFT SHIFTRIGHT SUP SUPEQ INF INFEQ DOUBLEQUAL DIF
%nonassoc EOF LPARENT RPARENT LBRACE RBRACE SEMICOLON COLON ARROWFUNC TRIPLEDOT EQUAL Integer_lit String_lit
%nonassoc UMINUS NOT
%nonassoc ENUM EXTERNAL SIG FUNCTION STRUCT TRUE FALSE EMPTY SWITCH IF ELSE FOR CONST VAR

%start prog


%type <Ast.ktype> ktype
%type <Ast.program> prog

%%

prog:
    | pns = list(prog_nodes) EOF { Prog pns }
;;

prog_nodes:
    | enum_decl { NEnum $1 }
    | struct_decl { NStruct $1 }
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
