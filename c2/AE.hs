module AE where 

import Test.HUnit 

-- consumes an AE and computes the corresponding number
calc :: AE -> Integer

-- some HUnit test cases to better understand the calc semantics

exp1, exp2 :: String 
exp1 = "Num 3"
exp2 = "Add (Num 3) (Sub (Num 10) (Num 5))" 

tc1 = TestCase (assertEqual "tc01" (calc (parse exp1)) 3) 

tc2 = TestCase (assertEqual "tc02" (calc (parse exp2)) 8) 

tests = TestList [TestLabel "Num 3 == 3" tc1, TestLabel "3 + (10 - 5) == 8" tc2]
 
data AE = Num Integer 
        | Add AE AE
        | Sub AE AE
 deriving(Read, Show, Eq)
calc (Num n) = n
calc (Add lhs rhs) = calc lhs + calc rhs
calc (Sub lhs rhs) = calc lhs - calc rhs

parse :: String -> AE
parse s = read s 

-- To run tests, at a Haskell interpreter prompt use
-- > runTestTT tests
