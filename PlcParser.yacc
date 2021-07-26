%%

%name PlcParser

%pos int


%term VAR
    | PLUS | MINUS | MULT | DIV | EQ
    | LPAR | RPAR
    | SEMIC
    | NAME of string | CINT of int
    | EOF

%nonterm Prog of expr | Expr of expr | AtomExpr of expr | Const of expr

%right SEMIC
%left EQ PLUS MINUS MULT DIV

%eop EOF

%noshift EOF

%start Prog

%%

Prog : Expr (Expr)
    | VAR NAME EQ Expr SEMIC Prog (Let(NAME, Expr, Prog))

Expr : AtomExpr (AtomExpr)
    | Expr PLUS Expr (Prim2("+", Expr1, Expr2))
    | Expr MINUS Expr (Prim2("-", Expr1, Expr2))
    | Expr MULT Expr (Prim2("*", Expr1, Expr2))
    | Expr DIV Expr (Prim2("/", Expr1, Expr2))
    | Expr EQ Expr (Prim2("=", Expr1, Expr2))

AtomExpr : Const (Const)
    | NAME (Var(NAME))
    | LPAR Expr RPAR (Expr)

Const : CINT (ConI(CINT))