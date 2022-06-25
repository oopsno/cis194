import Data.List (sort)
import Life
import Test.HUnit

allTests =
  TestList
    [ "neighbors" ~: testNeighbors,
      "shouldBeAlive" ~: testShouldBeAlive,
      "yourConcat" ~: testYourConcat,
      "nextGeneration" ~: testNextGeneration,
      "generations" ~: testGenerations
    ]

testNeighbors =
  TestList
    [ sort (neighbors (0, 0))
        ~?= sort [(-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1)]
    ]

testShouldBeAlive =
  TestList
    [ shouldBeAlive (0, 0) [] ~?= False,
      shouldBeAlive (0, 0) [(-1, -1), (0, -1), (1, -1)] ~?= True
    ]

testYourConcat =
  TestList
    [ yourConcat [[2, 1], [], [5], [7]] ~?= [2, 1, 5, 7]
    ]

testNextGeneration =
  TestList
    [ sort (nextGeneration [(-1, 0), (0, 0), (1, 0)])
        ~?= sort [(0, -1), (0, 0), (0, 1)],
      nextGeneration [(0, 0)] ~?= []
    ]

testGenerations =
  TestList
    [ (sort <$> generations 3 glider)
        ~?= ( sort
                <$> [ [(0, -1), (1, 0), (-1, 1), (0, 1), (1, 1)],
                      [(1, 0), (0, 1), (1, 1), (-1, 0), (0, 2)],
                      [(1, 0), (1, 1), (0, 2), (-1, 1), (1, 2)]
                    ]
            )
    ]

main :: IO Counts
main = runTestTT allTests
