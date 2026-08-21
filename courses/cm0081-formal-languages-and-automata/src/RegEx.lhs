% Some Algorithms on Regular Expressions

Adapted from a course by Thierry Coquand.

Tested with GHC 9.12.2.

A polymorphic regular expression data type
------------------------------------------

\begin{code}

module RegEx where

import Prelude hiding ( isInfinite )

data RegExp a = Empty
              | Epsilon
              | Symbol a
              | Star (RegExp a)
              | Plus (RegExp a) (RegExp a)
              | Dot  (RegExp a) (RegExp a)
\end{code}

Test if L(E) = ∅
----------------

\begin{code}
isEmpty :: RegExp a -> Bool
isEmpty Empty        = True
isEmpty (Plus e1 e2) = isEmpty e1 && isEmpty e2
isEmpty (Dot e1 e2)  = isEmpty e1 || isEmpty e2
isEmpty _            = False
\end{code}

Test if ε ∈ L(E)
----------------

\begin{code}
hasEpsilon :: RegExp a -> Bool
hasEpsilon Epsilon      = True
hasEpsilon (Star _)     = True
hasEpsilon (Plus e1 e2) = hasEpsilon e1 || hasEpsilon e2
hasEpsilon (Dot e1 e2)  = hasEpsilon e1 && hasEpsilon e2
hasEpsilon _            = False
\end{code}

Test if L(E) ⊆ {ε}
------------------

\begin{code}
atMostEpsilon :: RegExp a -> Bool
atMostEpsilon Empty        = True
atMostEpsilon Epsilon      = True
atMostEpsilon (Star e)     = atMostEpsilon e
atMostEpsilon (Plus e1 e2) = atMostEpsilon e1 && atMostEpsilon e2
atMostEpsilon (Dot e1 e2)  =
  isEmpty e1 || isEmpty e2 || (atMostEpsilon e1 && atMostEpsilon e2)
atMostEpsilon _  = False
\end{code}

Test if L(E) is infinite
------------------------

\begin{code}
isNotEmpty :: RegExp a -> Bool
isNotEmpty = not . isEmpty

isInfinite :: RegExp a -> Bool
isInfinite (Star e)     = not $ atMostEpsilon e
isInfinite (Plus e1 e2) = isInfinite e1 || isInfinite e2
isInfinite (Dot e1 e2)  =
  (isInfinite e1 && isNotEmpty e2) || (isNotEmpty e1 && isInfinite e2)
isInfinite _ = False
\end{code}

Compute the derivative of a regular expression by a symbol
-----------------------------------------------------------

\begin{code}
reDer :: Eq a => RegExp a -> a -> RegExp a
reDer (Symbol t)   s = if t == s then Epsilon else Empty
reDer (Plus e1 e2) s = reDer e1 s `Plus` reDer e2 s
reDer (Star e)     s = reDer e s `Dot` Star e

reDer (Dot e1 e2)  s =
  if hasEpsilon e1
  then (reDer e1 s `Dot` e2) `Plus` reDer e2 s
  else reDer e1 s `Dot` e2

reDer _ _ = Empty
\end{code}

Compute the derivative of a regular expression by a string
-----------------------------------------------------------

\begin{code}
reDerW :: Eq a => RegExp a -> [a] -> RegExp a
reDerW e []       = e
reDerW e (x : xs) = reDer (reDerW e xs) x
\end{code}

Test if w ∈ L(E)
----------------

\begin{code}
inRE :: Eq a => RegExp a -> [a] -> Bool
inRE e xs = hasEpsilon (reDerW e xs)
\end{code}

Common used regular expressions
-------------------------------

\begin{code}
type RE = RegExp Char

-- 0 and 1.
_0, _1 :: RE
_0 = Symbol '0'
_1 = Symbol '1'

-- 00, 01, 10 and 11.
_00, _01, _10, _11 :: RE
_00 = _0 `Dot` _0
_01 = _0 `Dot` _1
_10 = _1 `Dot` _0
_11 = _1 `Dot` _1

-- O^* and 1^*.
_0s, _1s :: RE
_0s = Star _0
_1s = Star _1

-- (0+1)^*.
universe :: RE
universe = Star (Plus _0 _1)

-- (01)^* and (10)^*.
_0s1s, _1s0s :: RE
_0s1s = Star (Dot _0 _1)
_1s0s = Star (Dot _1 _0)
\end{code}

Examples of regular expressions
-------------------------------

Alternating `0`'s and `1`'s: `(01)^* + (10)^* + 0(10)^* + 1(01)^*`.

\begin{code}
re1 :: RE
re1 = _0s1s `Plus` _1s0s `Plus` (_0 `Dot` _1s0s) `Plus` (_1 `Dot` _0s1s)
\end{code}

The set `{ x01y | x,y ∈ {0,1}^* }: (0 + 1)^*01(0 + 1)^*`.

\begin{code}
re2 :: RE
re2 = universe `Dot` _01 `Dot` universe
\end{code}

Strings over `Σ = {0,1}` such that there are two `0`’s separated by a
number of positions that is multiple of 2. Note that `0` is an
allowable multiple of 2. (Exercise 2.3.4c). `(0 + 1)^*0(01 + 10 + 11)^*0(0 + 1)^*`.

\begin{code}
re3 :: RE
re3 = universe
      `Dot` _0
      `Dot` Star (_01 `Plus` _10 `Plus` _11)
      `Dot` _0
      `Dot` universe
\end{code}

* * * * *

Generated with [pandoc](https://pandoc.org/) 3.9 and
[pandoc.css](https://gist.github.com/killercup/5917178) running the
following command:

```bash
$ pandoc -f markdown+lhs \
         -s \
         --highlight-style pygments \
         --css pandoc.css \
         -o RegEx.html \
         RegEx.lhs
```
