{ 
module GrammarCW where 
import TokensCW 
}

%name parseCW
%tokentype { Token } 
%error { parseError }
%token 
    put { TokenPut } 
    if  { TokenIf}
    else {TokenElse}
    int { TokenInt $$ } 
    setval {TokenSetVal}
    untilend { TokenUntilEnd}
    pass {TokenPass}
    discard {TokenDiscard}
    lesseq {TokenLessEq}
    greatereq {TokenGreaterEq}
    getval {TokenGetVal}
    read { TokenRead}
    for  { TokenFor }
    while {TokenWhile}
    diff {TokenNotEq}
    and  {TokenAnd}
    or  {TokenOr}
    '=' { TokenEq }
    '<' { TokenLessThan } 
    '>' { TokenGreaterThan}
    '+' { TokenPlus  } 
    '-' {TokenMinus}
    '*' {TokenTimes}
    '/' {TokenDiv}
    '%' { TokenMod}
    '(' { TokenLParen } 
    ')' { TokenRParen }  
    '{' {TokenLBracket}
    '}' {TokenRBracket} 
    ';' { TokenSemi} 


   



%right in
%left '+' '-'
%left '*' '/' '%'
%left NEG
%left and
%left or
%%
Lang : Exp Lang {Do $1 $2}
     | Exp      {Do1 $1}

Exp: put int '(' Value ')' {Put $2 $4}
   | setval '(' Value ')' '(' Value ')' {Set $3 $6} 
   | discard int {Discard $2}
   | pass {Pass}
   | untilend int '{' Lang '}' {UntilEnd $2 $4}
   | for '(' int '=' Value ';' Cond ';' Value ')' '{' Lang '}' {For $3 $5 $7 $9 $12} 
   | while '(' Cond ')' '{' Lang '}' {While $3 $6}
   | if Cond '{' Lang '}' else '{'Lang '}' {If $2 $4 $8}

Cond: Value '<' Value {Less $1 $3}
    | Value '>' Value {Greater $1 $3}
    | Value lesseq Value {LessEq $1 $3}
    | Value greatereq Value {GreaterEq $1 $3}
    | Value '=' Value {Eq $1 $3}
    | Value diff Value {NotEq $1 $3}
    | Cond and Cond {And $1 $3}
    | Cond or Cond  {Or $1 $3}
    | '(' Cond ')' {$2}

Value: int {Int1 $1}
     | getval '(' Value ')'  {Get $3}
     | read int  {Read $2}
     | Arithmetic {$1}


Arithmetic : Value '+' Value {Plus $1 $3}
            |Value '-' Value {Minus $1 $3}
            |Value '*' Value {Times $1 $3}
            |Value '/' Value {Div $1 $3}
            |'('Value ')'{$2}
            |'-' Value %prec NEG      {Negate $2 } 
            |Value '%' Value {Modulo $1 $3}

{ 
parseError :: [Token] -> a
parseError _ = error "Parse error" 

data Lang = Do Exp Lang | Do1 Exp deriving (Show, Eq)

data Exp = Put Int Value | Set Value Value | Discard Int | UntilEnd Int Lang | While Cond Lang | For Int Value Cond Value Lang | For1 Int Cond Int Lang | Pass | If Cond Lang Lang deriving (Show, Eq)

data Cond = Less Value Value |Greater Value Value| LessEq Value Value | Or Cond Cond | And Cond Cond | GreaterEq Value Value | Eq Value Value | NotEq Value Value deriving (Show, Eq)

data Value = Int1 Int | Get Value | Read Int | Plus Value Value | Minus Value Value | Times Value Value | Div Value Value | Modulo Value Value | Negate Value deriving (Show, Eq, Ord)



} 
