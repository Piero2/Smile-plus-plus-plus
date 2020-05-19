module Main where

import TokensCW
import GrammarCW
import System.Environment
import Control.Exception
import System.IO
import Data.List



main :: IO ()
main = catch main' noParse

main' = do
           (fileName : _ ) <- getArgs
           sourceText <- readFile fileName
           inputS <- getContents
           let separated1 = split '\n' inputS
           let separated2 = map (split ' ') separated1
           let matrix =  [map (\x -> (read x :: Int)) y | y <- separated2]
           let sourceText2 = map mapChar sourceText
           let inputArray = reverseAll $ transpose matrix
	      --putStrLn("Input matrix is " ++ show inputArray)
           let parsedProg = parseCW (alexScanTokens sourceText2)
           --putStrLn("Final parsing result = " ++ show parsedProg)
           format $ transpose $ resizeTo (length(inputArray!!0)) $ cutUnused $ getAccumulator $ eval parsedProg inputArray



--splitaux d t s [] = t
--splitaux d t s (c:cs) | (c == d) = splitaux d (t:s) "" cs
--					  | otherwise = splitaux d t (s:c) cs


reverseAll :: [[Int]] -> [[Int]]
reverseAll [] = []
reverseAll (xs:xss) = ((reverse xs) : (reverseAll xss))

splitaux2 :: Char -> String -> String -> [String] -> [String]
splitaux2 d [] [] acc = acc
splitaux2 d [] curr acc = (curr:acc)
splitaux2 d (c:cs) curr acc | (c == d) = splitaux2 d cs "" (curr:acc)
                            | otherwise = splitaux2 d cs (c:curr) acc

split :: Char -> String -> [String]
split d cs = splitaux2 d cs [] []


eval :: Lang -> [[Int]] -> ([Int], [Int], [[Int]])
eval lang input = evalLang lang input (getDefaultHeads (length input)) (getVars 100000) (getDefaultAccumulator 50)

format :: [[Int]] -> IO ()
format (xs:xss) = do
                    format2 xs
                    format xss
format (xss) = do putStr("")

format2 :: [Int] -> IO ()
format2 (x:xs) = do
                    putStr (show x)
                    putStr (" ")
                    format2 xs
format2 xs = do putStr("\n")

getVars :: Int -> [Int]
getVars 0 = []
getVars x = 0 : (getVars (x-1))

getDefaultHeads :: Int -> [Int]
getDefaultHeads 0 = []
getDefaultHeads x = 0 : (getDefaultHeads (x-1))

getDefaultAccumulator :: Int -> [[Int]]
getDefaultAccumulator 0 = []
getDefaultAccumulator x = [] : (getDefaultAccumulator (x-1))

resizeTo :: Int -> [[Int]] -> [[Int]]
resizeTo size (xs:xss) = (resizeTo2 size xs) : (resizeTo size xss)
resizeTo size (xss) = []

resizeTo2 :: Int -> [Int] -> [Int]
resizeTo2 0 (xs) = []
resizeTo2 size []     = 0 : (resizeTo2 (size-1) [])
resizeTo2 size (x:xs) = x : (resizeTo2 (size-1) xs)

-- eval takes list of instructions, initial streams, pos of heads, variables and partial streams
-- and returns new heads, variables and partials
evalLang :: Lang -> [ [Int] ] -> [Int] -> [Int] -> [[Int]] -> ([Int], [Int], [[Int]])
evalLang (Do1 what) input heads variables acc = evalExp what input heads variables acc
evalLang (Do what whatNext) input heads variables acc = evalLang whatNext input newHeads newVariables newAccumulator
    where
        result = (evalExp what input heads variables acc)
        newHeads = getHeads result
        newVariables = getVariables result
        newAccumulator = getAccumulator result

evalExp :: Exp -> [[Int]] -> [Int] -> [Int] -> [[Int]] -> ([Int], [Int], [[Int]])


evalExp Pass input heads variables acc = (heads,variables,acc)

evalExp(If c lt lf) input heads variables acc | (snd $ evaluation) == True = evalLang lt input newHeads variables acc
                                              | otherwise = evalLang lf input newHeads variables acc
                                               where
                                                   evaluation = evalCond c input heads variables
                                                   newHeads = (fst $ evaluation)

evalExp (Discard a)  input heads variables acc = (newHeads ,variables ,acc)
                                                where
                                                    newHeads = increaseHead heads a

evalExp (Set var val) input heads variables acc = (newHeads, newVariables, acc)
    where
	firstValues = evalValue var input heads variables
	actualVar = snd $ firstValues
	newHeads1 = fst $ firstValues
        newHeads = fst $ evaluation
        newVariables = getSetVariables variables actualVar (snd $ evaluation )
        evaluation = evalValue val input newHeads1 variables

evalExp (Put str val) input heads variables acc = (newHeads , variables, newAccumulator)
                        where
                            newHeads = fst $ evaluation
                            newAccumulator = getPutAccumulator acc str (snd evaluation)
                            evaluation = evalValue val input heads variables
evalExp (UntilEnd str lang) input heads variables acc | streamIsOver = (heads,variables,acc)
                                                      | otherwise    = evalExp (UntilEnd str lang) input newHeads newVars newAccumulator
    where
        streamIsOver = (heads!!str) >= (length (input!!str))
        newValues = evalLang lang input heads variables acc
        newHeads = getHeads newValues
        newVars = getVariables newValues
        newAccumulator = getAccumulator newValues

evalExp (While cond lang) input heads variables acc | not checkSatisfied = (heads, variables, acc)
                                                    | otherwise = evalExp (While cond lang) input newHeads newVars newAccumulator
    where
    	checkResult = evalCond cond input heads variables
    	checkSatisfied = snd $ checkResult
    	newHeads1 = fst $ checkResult
    	langResult= evalLang lang input newHeads1 variables acc
    	newHeads = getHeads langResult
        newVars = getVariables langResult
        newAccumulator = getAccumulator langResult

evalExp(For var initial check step lang) input heads variables acc = evalExp (For1 var check realStep lang) input newHeads newVars acc
    where
        newVars = setVar variables var inValue
        inReturn = evalValue initial input heads variables
        inValue = snd $ inReturn
        newHeads1 = fst $ inReturn
        realStep1 = evalValue step input newHeads1 newVars
        newHeads = fst $ realStep1
        realStep = snd $ realStep1

evalExp (For1 var check step lang) input heads variables acc | not checkSatisfied = (heads, variables, acc)
                                                             | otherwise = evalExp (For1 var check step lang) input newHeads newVars newAccumulator
    where
        checkSatisfied = snd $ (evalCond check input heads variables)
        newValues1 = evalLang lang input heads variables acc
        newHeads = getHeads newValues1
        newVars1 = getVariables newValues1
        newAccumulator = getAccumulator newValues1
        newVars = increaseVar newVars1 var step

evalCond :: Cond -> [[Int]] -> [Int] -> [Int] -> ([Int], Bool)

evalCond (And a b) input heads variables = (newHeads2, boolResult)
	where
		evalofA = evalCond a input heads variables
		actualValueA = snd evalofA
		newHeads = fst evalofA
		evalofB = evalCond b input newHeads variables
		actualValueB = snd evalofB
		newHeads2 = fst evalofB
		boolResult = actualValueA && actualValueB

evalCond (Or a b) input heads variables = (newHeads2, boolResult)
	where
		evalofA = evalCond a input heads variables
		actualValueA = snd evalofA
		newHeads = fst evalofA
		evalofB = evalCond b input newHeads variables
		actualValueB = snd evalofB
		newHeads2 = fst evalofB
		boolResult = actualValueA || actualValueB

evalCond (Less a b) input heads variables = (newHeads2, boolResult)
    where
        evalofA = evalValue a input heads variables
        newHeads = fst evalofA
        actualValueA = snd evalofA
        evalofB = evalValue b input newHeads variables
        actualValueB = snd evalofB
        newHeads2 = fst evalofB
        boolResult = actualValueA < actualValueB

evalCond (Greater a b) input heads variables = (newHeads2, boolResult)
    where
        evalofA = evalValue a input heads variables
        newHeads = fst evalofA
        actualValueA = snd evalofA
        evalofB = evalValue b input newHeads variables
        actualValueB = snd evalofB
        newHeads2 = fst evalofB
        boolResult = actualValueA > actualValueB

evalCond (LessEq a b) input heads variables = (newHeads2, boolResult)
    where
        evalofA = evalValue a input heads variables
        newHeads = fst evalofA
        actualValueA = snd evalofA
        evalofB = evalValue b input newHeads variables
        actualValueB = snd evalofB
        newHeads2 = fst evalofB
        boolResult = actualValueA <= actualValueB

evalCond (GreaterEq a b) input heads variables = (newHeads2, boolResult)
    where
        evalofA = evalValue a input heads variables
        newHeads = fst evalofA
        actualValueA = snd evalofA
        evalofB = evalValue b input newHeads variables
        actualValueB = snd evalofB
        newHeads2 = fst evalofB
        boolResult = actualValueA >= actualValueB

evalCond (Eq a b) input heads variables = (newHeads2, boolResult)
    where
        evalofA = evalValue a input heads variables
        newHeads = fst evalofA
        actualValueA = snd evalofA
        evalofB = evalValue b input newHeads variables
        actualValueB = snd evalofB
        newHeads2 = fst evalofB
        boolResult = actualValueA == actualValueB

evalCond (NotEq a b) input heads variables = (newHeads2, boolResult)
    where
        evalofA = evalValue a input heads variables
        newHeads = fst evalofA
        actualValueA = snd evalofA
        evalofB = evalValue b input newHeads variables
        actualValueB = snd evalofB
        newHeads2 = fst evalofB
        boolResult = actualValueA /= actualValueB

evalValue :: Value -> [[Int]] -> [Int] -> [Int] -> ([Int],Int)

evalValue (Get a) input heads variables = (newHeads,variables!!actualVarNumber)
	where
		evaluation = evalValue a input heads variables
		actualVarNumber = snd $ evaluation
		newHeads = fst $ evaluation

evalValue (Int1 a) input heads variables = (heads, a)

evalValue (Negate a) input heads variables = (newHeads, -actualValueA)
    where
        evalofA = evalValue a input heads variables
        newHeads = fst evalofA
        actualValueA = snd evalofA

evalValue (Plus a b) input heads variables = (newHeads2, actualValueA+actualValueB)
    where
        evalofA = evalValue a input heads variables
        newHeads = fst evalofA
        actualValueA = snd evalofA
        evalofB = evalValue b input newHeads variables
        actualValueB = snd evalofB
        newHeads2 = fst evalofB

evalValue (Minus a b) input heads variables = (newHeads2, actualValueA-actualValueB)
    where
        evalofA = evalValue a input heads variables
        newHeads = fst evalofA
        actualValueA = snd evalofA
        evalofB = evalValue b input newHeads variables
        actualValueB = snd evalofB
        newHeads2 = fst evalofB

evalValue (Times a b) input heads variables = (newHeads2, actualValueA*actualValueB)
    where
        evalofA = evalValue a input heads variables
        newHeads = fst evalofA
        actualValueA = snd evalofA
        evalofB = evalValue b input newHeads variables
        actualValueB = snd evalofB
        newHeads2 = fst evalofB

evalValue (Div a b) input heads variables = (newHeads2, actualValueA `div` actualValueB)
    where
        evalofA = evalValue a input heads variables
        newHeads = fst evalofA
        actualValueA = snd evalofA
        evalofB = evalValue b input newHeads variables
        actualValueB = snd evalofB
        newHeads2 = fst evalofB

evalValue (Modulo a b) input heads variables = (newHeads2, actualValueA `mod` actualValueB)
    where
        evalofA = evalValue a input heads variables
        newHeads = fst evalofA
        actualValueA = snd evalofA
        evalofB = evalValue b input newHeads variables
        actualValueB = snd evalofB
        newHeads2 = fst evalofB

evalValue (Read a) input heads variables = (newHeads,input!!a!!(heads!!a))
    where newHeads = increaseHead heads a




mapChar '\n' = ' '
mapChar c = c

getHeads (a,_,_) = a

getVariables (_,a,_) = a

getAccumulator (_,_,a) = a

increaseHead :: [Int] -> Int -> [Int]
increaseHead (x:xs) 0 = ((x+1):xs)
increaseHead (x:xs) whichHead = (x:(increaseHead xs (whichHead-1)))

increaseVar :: [Int] -> Int -> Int -> [Int]
increaseVar (x:xs) 0 step = ((x+step):xs)
increaseVar (x:xs) whichVar step = (x: ( increaseVar xs (whichVar-1) step ) )

setVar :: [Int] -> Int -> Int -> [Int]
setVar (x:xs) 0 value = (value:xs)
setVar (x:xs) whichVar value = (x:(setVar xs (whichVar-1) value ))

getSetVariables (x:xs) 0 val = (val:xs)
getSetVariables (x:xs) var val =  (x:(getSetVariables xs (var-1) val))

getPutAccumulator :: [[Int]] -> Int -> Int -> [[Int]]
getPutAccumulator (xs:xss) 0 val = ((xs++[val]):xss)
getPutAccumulator (xs:xss) str val = (xs:(getPutAccumulator xss (str-1) val))

cutUnused :: [[Int]] -> [[Int]]
cutUnused ([]:xss) = cutUnused xss
cutUnused (xs:xss) = xs : cutUnused xss
cutUnused (xss) = []

noParse :: ErrorCall -> IO ()
noParse e = do let err =  show e
               hPutStr stderr err
               return ()
