module LogicNinetyNine
    ( myAnd,
        myOr,
        myNand,
        myNor,
        myXor
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