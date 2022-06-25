module Life where

import Data.Function (on)
import Data.List (groupBy, intercalate, nub, sort)
import Data.Ord (comparing)
import Data.Semigroup ((<>))
import Control.Monad (ap)

{- 3.1 -- Let the types guide you -}

{-
Fill in the definition for the following functions. Look at the types,
no further instructions necessary.
-}
foo :: a -> b -> a
foo a _ = a

bar :: a -> (a, a)
bar a = (a, a)

baz :: (a -> b) -> a -> b
baz f = f

{- 3.2 -- Conway's Game of Life -}

{-
The universe of the Game of Life is an infinite, two-dimensional orthogonal
grid of square cells, each of which is in one of two possible states, alive or
dead, (or populated and unpopulated, respectively). Every cell interacts with
its eight neighbours, which are the cells that are horizontally, vertically,
or diagonally adjacent. At each step in time, the following transitions occur:

  1. Any live cell with 2 or 3 live neighbors lives on to the next generation.
  2. Any dead cell with exactly 3 live neighbors becomes a live cell.
  3. All other cells are dead in the next generation.

The initial pattern constitutes the seed of the system. The first generation is
created by applying the above rules simultaneously to every cell in the seed;
births and deaths occur simultaneously, and the discrete moment at which this
happens is sometimes called a tick. Each generation is a pure function of the
preceding one. The rules continue to be applied repeatedly to create further
generations.

Source: https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life
-}

{-
The way that we are going to model the state of the game is by keeping track
of the cells that are alive in a list. Each cell will be identified by an
an `(Int, Int)` tuple where the components correspond to the x and y position
of the cell respectively.

Since many of the functions below take in and return cells, it would be handy
if we could make a shorthand for the type `(Int, Int)`. Luckily Haskell allows
us to do this using a functionality called type synonyms. Essentially, you
give a name to some arbitrary type which you can then use in place of that
original type when you are writing type annotations. Here we create a type
synonym `Cell`. Wherever you the type `Cell` below, you can mentally replace
it with `(Int, Int)` without changing the meaning of the program. Likewise
with `Generation`.
-}

type Cell = (Int, Int)

type Generation = [Cell]

{-
The first step is to create a function that can give us the neighbors of a
given cell. Remember to consider the neighbors of a cell horizontally,
vertically, and diagonally. Make sure not to include the cell itself though
as it is not its own neighbor!

The `where` clause allows us to create local definitions that we can use for
intermediate computations or even helper functions. Here we use it to
deconstruct the cell into its x and y components. Now you can use `x` and `y`
within the body of the function instead of calling `fst` and `snd` repeatedly.
Note that you do not need to modify any part of the definition after the
`where` clause-- take that code as given.
-}
neighbors :: Cell -> [Cell]
neighbors cell = [(x', y') | x' <- [x - 1 .. x + 1], y' <- [y - 1 .. y + 1], (x', y') /= (x, y)]
  where
    x = fst cell
    y = snd cell

{-
This next function will take in a cell and the list of cells that are alive and
tell you whether that cell should be alive in the next generation. Refer to the
rules above for the exact logic.

The recommended way to do this is to first define `numAliveNeighbors` in the
`where` clause which will be the number of neighbors of the `cell` that are
currently alive. I recommend using function composition for this problem
(the `.` and `$` operators). If you are feeling stuck, look at the posted
slides from lec2.

One function that will be very useful is `elem :: a -> [a] -> Bool` which
tells you whether the provided element is contained within the provided list.
-}
shouldBeAlive :: Cell -> Generation -> Bool
shouldBeAlive cell gen =
  if cell `elem` gen
    then numAliveNeighbors == 2 || numAliveNeighbors == 3
    else numAliveNeighbors == 3
  where
    numAliveNeighbors :: Int
    numAliveNeighbors = length $ filter (`elem` gen) (neighbors cell)

{-
Given a list of the cells that are alive `potentiallyAliveCells` will return a
list of the cells that could possibly be alive in the next generation. In other
words, this function will give us the list of cells we need to check when
computing the next generation. We are giving you the implementation of this
function as a freebie. You should still read it and try to understand
how it works.

In order to write this function, it is helpful to first implement `yourConcat`
which takes a list of lists and flatten it to a list. This one is on
you to implement.

    yourConcat [[2, 1], [], [5], [7]] --> [2,1,5,7]

Once again, we use the `your` prefix because the Haskell standard library
includes a function named `concat`. Don't use that function in your solution!
-}
yourConcat :: [[a]] -> [a]
yourConcat = foldr (++) []

potentiallyAliveCells :: Generation -> [Cell]
potentiallyAliveCells aliveCells =
  nub $ aliveCells ++ yourConcat (map neighbors aliveCells)

{-
Given a list of cells that are alive in this generation, this function will
give you a list of cells that are alive in the next generation. Once again,
function composition will be useful. If you want, you can even try writing
this function using point-free style.
-}
nextGeneration, nextGeneration' :: Generation -> Generation
nextGeneration g = filter (`shouldBeAlive` g) $ potentiallyAliveCells g
nextGeneration' = ap (filter . flip shouldBeAlive) potentiallyAliveCells

{-
The last function that you need to write will take in a number and initial seed
and return a list of that many generations. Since each generation is
represented as a list of cells that are alive, the return type is

The two functions you should use here are `take : Int -> [a] -> [a]` and
`iterate :: (a -> a) -> a -> [a]`. Read the Haskell documentation for more
information about what these functions do.
-}
generations :: Int -> Generation -> [Generation]
generations steps init = take steps $ iterate nextGeneration init

{-
Here is a seed that is commonly called glider because some people think its
motion resembles a glider in flight. You can use it to test your program.
-}
glider :: [Cell]
glider = [(0, -1), (1, 0), (-1, 1), (0, 1), (1, 1)]
