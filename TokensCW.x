{ 
module TokensCW where 
}

%wrapper "basic" 
$digit = 0-9     
-- digits 
$alpha = [a-zA-Z]    
-- alphabetic characters

tokens :-
$white+       ; 
  "//".[^\\]*."\\"        ; 
  $digit+     { \s -> TokenInt (read s) } 
  \<          { \s -> TokenLessThan }
  \>          { \s -> TokenGreaterThan}
  if          { \s -> TokenIf }
  \(          { \s -> TokenLParen }
  \)          { \s -> TokenRParen }
  \{          { \s -> TokenLBracket }
  \}          { \s -> TokenRBracket }
  else        { \s -> TokenElse}
  put         { \s -> TokenPut}
  \/          { \s -> TokenDiv}
  \+          { \s -> TokenPlus} 
  \-          { \s -> TokenMinus} 
  \*          { \s -> TokenTimes} 
  \=          { \s -> TokenEq}
  discard     { \s -> TokenDiscard}
  setValue    { \s -> TokenSetVal}  
  pass        { \s -> TokenPass}
  until_end   { \s -> TokenUntilEnd} 
  \<=         { \s -> TokenLessEq}
  \>=         { \s -> TokenGreaterEq}
  \!=         { \s -> TokenNotEq}
  getValue    { \s -> TokenGetVal}
  read        { \s -> TokenRead}
  \%          { \s -> TokenMod} 
  for         { \s -> TokenFor}
  \;          { \s -> TokenSemi}
  and         { \s -> TokenAnd}
  or          { \s -> TokenOr}
  while       { \s -> TokenWhile}


{ 
-- Each action has type :: String -> Token 
-- The token type: 
data Token = 
  TokenInt Int     | 
  TokenLessThan    | 
  TokenIf          |
  TokenLParen      |
  TokenGreaterEq   |
  TokenGreaterThan |
  TokenRParen      |
  TokenPut         |
  TokenLBracket    |
  TokenRBracket    |
  TokenSemi        |
  TokenElse        |
  TokenDiv         |
  TokenPlus        |
  TokenMinus       |
  TokenTimes       |
  TokenEq          |
  TokenDiscard     |
  TokenSetVal      |
  TokenPass        |
  TokenUntilEnd    |
  TokenEndLoop     |
  TokenEndIf       |
  TokenLessEq      |
  TokenNotEq       |
  TokenGetVal      |
  TokenRead        |
  TokenMod         |
  TokenFor         |
  TokenWhile       |
  TokenAnd         |
  TokenOr
  deriving (Eq,Show) 

}
