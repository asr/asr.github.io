-- Representation of an automaton using functional programming [1]
-- which accepts the binaries words with an even number of 0's and 1's
-- (see Example 2.4 from [2]).

-- [1] Robert M. Keller. Computer Science: Abstraction to
-- Implementation, 2001. http://www.cs.hmc.edu/~keller/cs60book/

-- [2] John E. Hopcroft, Rajeev Motwani, and Jefferey
-- D. Ullman. Introduction to Automata Theory, Languages, and
-- Computation. Pearson Education, Inc., 3rd edition, 2007.

-- Tested with GHC 9.8.1 and QuickCheck 2.14.3.

module Main where

import Test.QuickCheck
    ( Arbitrary(arbitrary)
    , classify
    , collect
    , elements
    , Property
    , quickCheck
    )

------------------------------------------------------------------------------
-- The automaton

-- The automaton accepts the word `xs` iff `q0 xs == True`.

-- | The input alphabet.
data Symbol = Zero | One
              deriving (Eq, Show)

type Symbols = [Symbol]

-- | The initial state.
q0 :: Symbols -> Bool
q0 []          = True   -- An accepting state
q0 (Zero : xs) = q2 xs
q0 (One : xs)  = q1 xs

q1 :: Symbols -> Bool
q1 []          = False  -- A non-accepting state
q1 (Zero : xs) = q3 xs
q1 (One : xs)  = q0 xs

q2 :: Symbols -> Bool
q2 []          = False  -- A non-accepting state
q2 (Zero : xs) = q0 xs
q2 (One : xs)  = q3 xs

q3 :: Symbols -> Bool
q3 []          = False  -- A non-accepting state
q3 (Zero : xs) = q1 xs
q3 (One : xs)  = q2 xs

------------------------------------------------------------------------------
-- QuickCheck tests [1].

-- [1] Koen Claessen and John Hughes. QuickCheck: A lightweight tool for
-- random testing of Haskell Programs. ICFP '00.

instance Arbitrary Symbol where
  arbitrary = elements [Zero, One]

-- Auxiliary functions

zerosEven :: Symbols -> Bool
zerosEven = even . length . filter (== Zero)

zerosOdd :: Symbols -> Bool
zerosOdd = not . zerosEven

onesEven :: Symbols -> Bool
onesEven = even . length . filter (== One)

onesOdd :: Symbols -> Bool
onesOdd = not . onesEven

evenZerosOnes :: Symbols -> Bool
evenZerosOnes xs = zerosEven xs && onesEven xs

-- Properties

helperProp :: Symbols -> Bool
helperProp xs = evenZerosOnes xs == q0 xs

prop_p1 :: Symbols -> Bool
prop_p1 = helperProp

prop_p2 :: Symbols -> Property
prop_p2 xs = classify (null xs) "trivial" $ helperProp xs

prop_p3 :: Symbols -> Property
prop_p3 xs = collect (length xs) $ helperProp xs

------------------------------------------------------------------------------
-- Main

main :: IO ()
main = do
  quickCheck prop_p1
  quickCheck prop_p2
  quickCheck prop_p3
