%%

%name PlcParser

%pos int


%term VAR
    | PLUS | MINUS | MULT | DIV | EQ | DIF | LESS | LESSEQ
    | NIL | BOOL of bool | INT of int
    | IF | THEN | ELSE | MATCH | WITH
    | AND | NOT
    | NEG | UND
    | CONCAT | HD | TL
    | ISE
    | PRINT
    | FUN | FN
    | LPAR | RPAR
    | LBRACE | RBRACE | LBRACKET | RBRACKET
    | COMMA | SEMIC | COLON
    | ARROW | ATFUN
    | NAME of string | CINT of int | CBOOL of bool
    | END | EOF

%nonterm Prog of expr | Expr of expr | AtomExpr of expr | Const of expr 
        | Decl of expr | Name of expr | Args of expr | Type of expr
        | AppExpr of expr | MatchExpr of expr | Nat of expr | Comps of expr
        | CondExpr of expr |  Params of expr | TypedVar of expr

%right SEMIC CONCAT
%left EQ DIF PLUS MINUS MULT DIV ELSE AND LESS LESSEQ LBRACKET
%nonassoc IF NOT HD TL ISE PRINT

%eop EOF

%noshift EOF

%start Prog

%%

Prog : Expr (Expr)
    | Decl SEMIC Prog (Let(NAME, Expr, Prog))

Decl : VAR NAME EQ Expr (Let(NAME, Expr, Expr))
    | FUN NAME Args EQ Expr (Let(NAME, Expr, Expr))
    | FUN REC NAME Args COLON Type EQ Expr (LetRec(NAME, Type, Args, Expr))

Expr : AtomExpr (AtomExpr)
    | AppExpr(AppExpr)
    | IF Expr1 THEN Expr2 ELSE Expr3 (If(Expr1, Expr2, Expr3))
    | MATCH Expr WITH MatchExpr (Expr, MatchExpr)
    | NOT Expr (Prim1("!", Expr1))
    | NEG Expr (Prim1("-", Expr1))
    | HD Expr (Prim1("hd", Expr1))
    | TL Expr (Prim1("tl", Expr1))
    | ISE Expr (Prim1("ise", Expr1))
    | PRINT Expr (Prim1("print", Expr1))
    | Expr AND Expr (Prim2("&&", Expr1, Expr2))
    | Expr PLUS Expr (Prim2("+", Expr1, Expr2))
    | Expr MINUS Expr (Prim2("-", Expr1, Expr2))
    | Expr MULT Expr (Prim2("*", Expr1, Expr2))
    | Expr DIV Expr (Prim2("/", Expr1, Expr2))
    | Expr EQ Expr (Prim2("=", Expr1, Expr2))
    | Expr DIF Expr (Prim2("!=", Expr1, Expr2))
    | Expr LESS Expr (Prim2("<", Expr1, Expr2))
    | Expr LESSEQ Expr (Prim2("<=", Expr1, Expr2))
    | Expr CONCAT Expr (Prim2("::", Expr1, Expr2))
    | Expr SEMIC Expr (Prim2(";", Expr1, Expr2))
    | Expr LBRACKET Nat RBRACKET (Nat)

AtomExpr : Const (Const)
    | NAME (Var(NAME))
    | LBRACE Prog RBRACE (Prog)
    | LPAR Expr RPAR (Expr)
    | LPAR Comps RPAR (Comps)
    | FN Args ATFUN Expr END (Args, Expr)

AppExpr : AtomExpr AtomExpr (AtomExpr)
    | AppExpr AtomExpr (AppExpr, AtomExpr)

Const : CBOOL (ConB(CBOOL))
    | CINT (ConI(CINT))
    | LPAR RPAR
    | LPAR Type LBRACKET RBRACKET RPAR

Comps : Expr(Expr) COMMA Expr (Expr)
    | Expr COMMA Comps (Expr, Comps)

MatchExpr : END
    | LMARK PIPE RMARK CondExpr ARROW Expr MatchExpr (CondExpr, Expr, MatchExpr)

CondExpr : Expr(Expr)
    | LMARK UND RMARK

Args : LPAR RPAR
    | LPAR Params RPAR (Params)

Params : TypedVar(TypedVar)
    | TypedVar COMMA Params

TypedVar : Type Name (Type, Name)

Type : AtomType (AtomType)
    | LPAR Types RPAR (Types)
    | LBRACKET Type RBRACKET (Type)
    | Type ARROW Type (Type, Type)

AtomType : NIL
    | BOOL
    | INT
    | LPAR Type RPAR (Type)

Types : Type COMMA Type (Type, Type)
    | Type COMMA Types (Type, Types)

