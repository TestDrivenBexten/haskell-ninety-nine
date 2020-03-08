module LogicNinetyNine
    ( myAnd,
        myOr,
        myNand,
        myNor,
        myXor,
        myImplies,
        myEqual
    ) where

myAnd :: Bool -> Bool -> Bool
myAnd a b = a && b

myOr :: Bool -> Bool -> Bool
myOr a b = a || b

myNand :: Bool -> Bool -> Bool
myNand a b = not (myAnd a b)

myNor :: Bool -> Bool -> Bool
myNor a b = not (myOr a b)

myXor :: Bool -> Bool -> Bool
myXor a b = not (myNand a b)

-- if a then b; a, therefore b
myImplies :: Bool -> Bool -> Bool
myImplies a b = myOr (not a) b

myEqual :: Bool -> Bool -> Bool
myEqual a b = a == b