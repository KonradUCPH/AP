-- Associativity, presidence, elimination of left recursion, left factoring
-- Grammar with epsilon moved to the end of the expression.

Expr        ::= Expr1 Expr'

Expr'       ::= ',' Expr | e

Expr1       ::= Ident '=' Expr1 
            | Expr2

Expr2       ::= Expr3 Expr2'

Expr2'      ::= '===' Expr3 | '<' Expr3 | e

Expr3       ::= Expr4 Expr3opt

Expr3opt    ::= '+' Expr4 Expr3opt 
            | '-' Expr4 Expr3opt
            | e

Expr4       ::= ExprX Expr4opt

Expr4opt    ::= '*' ExprX Expr4opt 
            | '%' ExprX Expr4opt
            | e

ExprX       ::= Number
            | String
            | 'true'
            | 'false'
            | 'undefined' 
            | ExprIdent
            | ExprBracet
            | '(' Expr ')'

ExprIdent   ::= Ident ExprIdent'

ExprIdent'  ::= '(' Exprs ')' | e

ExprBracet  ::= '[' ExprBracet'

ExprBracet' ::= Exprs ']' | ArrayFor ']'

Exprs       ::= Expr1 CommaExprs| e

CommaExprs  ::= ',' Expr1 CommaExprs | e

ArrayFor    ::= 'for' '(' Ident 'of' Expr1 ')' ArrayCompr 

ArrayIf     ::= 'if' '(' Expr1 ')' 

ArrayCompr  ::= Expr1
            | ArrayFor 
            | ArrayIf

Ident       ::=
Number      ::=
String      ::=