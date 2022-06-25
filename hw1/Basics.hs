module Basics where

import Data.Char (toUpper)

{- CIS 194 - Homework 1 - Basics.hs -}

{- 1.1 -- Tell us about yourself -}

{-
Let's start out by getting to know you a little bit.
(And you'll get some practice using the basic Haskell data types too!)

Think of each `undefined` as a todo. When you are finished with
the assignment there should be no `undefined`s left in your code.
-}

firstName :: String
firstName = "first"

lastName :: String
lastName = "last"

favoriteNumber :: Int
favoriteNumber = 42

{-
Okay, this next one is a little more interesting. The type annotation says that
`githubUsername` is a `Maybe String`. What does that mean? Well not everyone
has a GitHub account (although I encourage you to make one if you don't). How
can our data represent that?

A first idea would be to use an empty string for someone who doesn't have a
GitHub account. This seems fine but a potential problem arises when I try to
load your GitHub page by doing `"https://github.com/" ++ githubUsername`.
Instead of getting your page I will get the homepage. Sure I can fix this by
checking for empty strings whenever I use `githubUsername` but it is too easy
to forget to do that, especially if you are working in a large code base
dealing with code you didn't even write.

Can we do better? Perhaps there is a way to make the compiler force us to deal
with possibility of a missing GitHub account wherever `githubUsername` is used.
That's where `Maybe` comes in. We are going to talk a lot about `Maybe` in
lec3. For now just know that `Maybe` is a way to encode possible failure. If
the value exists, we can wrap it with `Just`. For example, `Just 5` is a
`Maybe Int`. In case no such value exists, we use `Nothing`.

(If you are familiar with OCaml, in that language we would write this type as
`string option` where `Just` and `Nothing` correspond to `Some` and `None`.)
-}

githubUsername :: Maybe String
githubUsername = Just "oopsno"

{- 1.2 -- The name of the game... -}

{-
Let's write a function to display a full name. The function should just
concatenate the first and last name given to it with a space in between.
Here are some examples to get you started:

    fullName "Haskell" "Curry" == "Haskell Curry"
    fullName "" "NoFirstName" == " NoFirstName"
    fullName "NoLastName" "" == "NoLastName "
    fullName "" "" == " "
-}

fullName :: String -> String -> String
fullName fname lname = fname ++ " " ++ lname

{-
The registrar just informed me that they actually need us to display last names
first. Particularly, the new format should be `lastName, firstName`.

    lastCommaFirst "George" "Washington" == "Washington, George"
-}

lastCommaFirst :: String -> String -> String
lastCommaFirst fname lname = lname ++ ", " ++ fname

{-
Alright, last one. This time we want to display initials, or the first letter
of a person's first name concatenated with the first letter of their lastname.

    initials "Ada" "Lovelace" == "AL"

How can we get the first letter of a `String`? There's a standard library
function `head` that returns the first element of whatever list you give it.
But we are working with `String`s not lists-- how does that help us? Well it
turns out that `String`s are just lists (and your whole life has been a lie).
Specifically, `String` is just shorthand for a list of `Char`s (we call this
a type alias in the biz).

So now we can get the first letter of a `String` but something is wrong. If
a `String` is a list of `Char`s and `head` gives us the first element of a list
then in our case that element will have type `Char`. That's where
`charToString` comes into play. It takes a single character and turns into a
singleton character list a.k.a. a `String`!

Oh, and one more thing! Make sure that `initials` always returns an uppercase
string. You can use `toUpper` for that which takes a character and returns
its uppercase version. Note that `toUpper` operates on `Char`s not `String`s!

    initials "alan" "turing" == "AT"

(Don't worry about what happens when either input is empty. Spoiler: it
probably isn't good. We can talk about this in lec2.)
-}

charToString :: Char -> String
charToString c = [c]

initials :: String -> String -> String
initials fname lname = upperHead fname ++ upperHead lname
  where
    upperHead = charToString . toUpper . head

{- 1.3 -- Lists, lists, and more lists -}

{-
As hinted to above, Haskell has lists. Haskell lists are homogenous (all
elements have the same type) and you can think of them as singly-linked if that
means anything to you. We are going to be talking about lists a lot this
semester but just to whet your appetite...
-}

primesLessThanTen :: [Int]
primesLessThanTen = [2, 3, 5, 7]

emptyList :: [Int]
emptyList = []

oddsBetweenZeroAndTen :: [Int]
oddsBetweenZeroAndTen = [x | x <- [0 .. 10], odd x]

{-
You can even have lists of lists assuming that the inner lists have the same
type of elements.
-}

listOfLists :: [[Int]]
listOfLists = [primesLessThanTen, emptyList, oddsBetweenZeroAndTen]

{-
You can tack on elements to the front of a list using the `(:)` operator, which
is pronounced cons.
-}

someEvenNumbers :: [Int]
someEvenNumbers = 2 : [6, 22, 14]

someOddNumbers :: [Int]
someOddNumbers = 1 : [3, 11, 77]

{-
By using multiple cons calls together we can build a list. `(:)` is
"right associative" which means that

    1 : 9 : 4 : [] == 1 : (9 : (4 : []))
                   == 1 : (9 : [4])
                   == 1 : [9, 4]
                   == [1, 9, 4]

But it is good style to omit the parentheses like so.
-}

oneTwoThree :: [Int]
oneTwoThree = 1 : 9 : 4 : [2]

{-
Haskell has a shorthand syntax for ranges of values. Observe:
-}

oneTwoThreeFourFive :: [Int]
oneTwoThreeFourFive = [1 .. 5]

lowercaseAlphabet :: [Char]
lowercaseAlphabet = ['a' .. 'z']

multiplesOfOneQuarterUpTo2 :: [Float]
multiplesOfOneQuarterUpTo2 = [0.25, 0.5 .. 2]

multiplesOfThreeUpTo20 :: [Int]
multiplesOfThreeUpTo20 = [3, 6 .. 20]

{-
There are also list comprehensions in Haskell. I do not particularly like them,
so this is probably the last time that I will mention them. You can read about
them in LYAH: http://learnyouahaskell.com/starting-out#im-a-list-comprehension.
Here is an example of a list comprehension:
-}

oddPowersOfTwo :: [Int]
oddPowersOfTwo = [2 ^ x | x <- [0 .. 10], odd x]

{- 1.4 -- Tuples -}

{-
Like most languages, Haskell has a tuple type. You can think of a tuple as a
pair of two related pieces of data. The two pieces of data stored in a tuple
do not need to be of the same type.
-}

whitehouseAddress :: (Int, String)
whitehouseAddress = (1600, "Pennsylvania Avenue")

{-
How can we access the components of our tuples? Typically, the best way to do
it is to use pattern matching. But for now we will use the projection functions
`fst` and `snd`. As the names might suggest, `fst` takes a tuple of size two
and returns the first component, while `snd` takes a tuple of size two and
returns the second component.

Let's use `fst` and `snd` to write the `addIntPairs` function which takes to
two tuples of size two and adds them component wise to produce a new tuple.

    addIntPairs ( 1, 2 ) ( 3, 4 ) == ( 4, 6 )
    addIntPairs ( -1, 3 ) ( 2, 0 ) == ( 1, 3 )
-}

addIntPairs :: (Int, Int) -> (Int, Int) -> (Int, Int)
addIntPairs pair1 pair2 = (fst pair1 + fst pair2, snd pair1 + snd pair2)

{-
Actually, you can store more than two pieces of data in a tuple. But just
because you can doesn't mean that you should. If you find yourself reaching
for a tuple of size greater than two, there is probably a better way to model
your data. We will talk at length about this starting with lec3.
Fun fact: GHC limits tuple size to 62 components. The nerve of some people!
-}

dontDoThis :: (Int, Bool, Int, Int, String, [Int], [Int], Double)
dontDoThis = (5, False, 23, 1, "hello", [], [101], 7.2)

{- END -}
