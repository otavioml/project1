%%

%name PlcParser

%pos int


%term VAR
    | PLUS | MINUS | MULT | DIV | EQ | LESS | LESSEQUAL | NEGATION | LOGICAND
    | LPAR | RPAR
    | SEMIC
    | NAME of string | CINT of int
    | EOF

%nonterm Prog of expr | Expr of expr | AtomExpr of expr | Const of expr

%right SEMIC
%left LOGICAND EQ LESS LESSEQUAL PLUS MINUS MULT DIV NEGATION

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
    | Expr LESS Expr (Prim2("<", Expr1, Expr2))
    | Expr LOGICAND Expr (Prim2("&&", Expr1, Expr2))
    | Expr LESSEQUAL Expr (Prim2("<=", Expr1, Expr2))
    | MINUS Expr (Prim1("-", Expr1))
    | NEGATION Expr (Prim1("!", Expr1))

AtomExpr : Const (Const)
    | NAME (Var(NAME))
    | LPAR Expr RPAR (Expr)

Const : CINT (ConI(CINT))