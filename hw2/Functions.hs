module Functions where

{- 2.1 -- Safety First -}

{-
Remember how we said that the way Prelude defines `head` and `tail` is pretty
much a mistake? Well let's fix that by creating "safe" variants of these
functions that do not crash. We will do this by using a `Maybe` such that we
return `Nothing` if an empty list was passed into the function.

    safeHead [] --> Nothing
    safeHead [3, 7, 9] --> Just 3
    safeTail [3, 7, 9] --> Just [7, 9]

While the above examples are a great start, make sure to test your functions on
a few more inputs too!
-}
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_ : xs) = Just xs

{- 2.2 -- Recursion -}

{-
There is a very elegant recursive algorithm for computing the greatest common
denominator of two numbers commonly known as Euclid's algorithm. In pseudocode,
the iterative version of the algorithm can be expressed as follows:

    function gcd(a, b)
      while b ≠ 0
        t := b;
        b := a mod b;
        a := t;
      return a;

    (Credit to Wikipedia for this particular version.)

Translate the above algorithm into a recursive one. For brownie points (i.e.
purely for fun), you can try proving that the algorithm terminates.

NOTE: The function below is named `yourGcd` because Prelude already defines a
`gcd` function and we do not want the names to clash. If you use that `gcd`
function in your solution, you will receive NO CREDIT for this problem!
-}
yourGcd :: Int -> Int -> Int
yourGcd a 0 = a
yourGcd a b = yourGcd b a `mod` b

{-
Let's implement `filter` for lists. As a reminder, `filter` takes a predicate
(a one argument function that returns `True` or `False`) and a list of elements
and returns a new list with all of the elements for which the predicate
returned `True` (in the same order as they appeared in the original list).

    filter (\x -> x < 4) [4, 2, 1, 1, 8, 3] --> [2, 1, 1, 3]
    filter odd [1..10] --> [1, 3, 5, 7, 9]

`filter` is an example of a "higher-order function." If you're feeling stuck,
try looking back at the implementation of `map` that we wrote together in lec2.
-}

intListFilter :: (Int -> Bool) -> [Int] -> [Int]
intListFilter predicate xs = [x | x <- xs, predicate x]

{- 2.3 -- Towers of Hanoi -}

{-
Adapted from an assignment given in UPenn CIS 552, taught by Benjamin Pierce.

The Towers of Hanoi is a classic puzzle with a solution that can be described
recursively. Disks of different sizes are stacked on three pegs; the goal is
to get from a starting configuration with all disks stacked on the left peg
to an ending configuration with all disks stacked on the right peg. The only
rules are:

    1. you may only move one disk at a time, and
    2. a larger disk may never be stacked on top of a smaller one.

For example, as the first move all you can do is move the topmost, smallest
disk onto a different peg, since only one disk may be moved at a time.

To move n discs (stacked in increasing size) from peg A to peg B
using peg C as temporary storage,

  1. move n - 1 disks from A to C using B as temporary storage
  2. move the top disc from A to B
  3. move n - 1 disks from C to B using A as temporary storage.

Given the number of discs and names for the three pegs, `hanoi` should
return a list of moves to be performed to move the stack of discs from
the first peg to the second.

    hanoi 0 "a" "b" "c" == []
    hanoi 1 "a" "b" "c" == [("a", "b")]
    hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]
-}
hanoi :: Integer -> String -> String -> String -> [(String, String)]
hanoi disks fromPeg toPeg storagePeg = hanoi' disks fromPeg toPeg storagePeg
  where
    hanoi' 0 _ _ _ = []
    hanoi' n a b c = hanoi' (n - 1) a c b ++ [(a, b)] ++ hanoi (n - 1) c b a

{- END -}
