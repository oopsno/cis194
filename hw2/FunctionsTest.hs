import Functions
import Test.HUnit

allTests =
  TestList
    [ "safeHead" ~: testSafeHead,
      "safeTail" ~: testSafeTail,
      "yourGcd" ~: testYourGcd,
      "intListFilter" ~: testIntListFilter,
      "hanoi" ~: testHanoi
    ]

testSafeHead =
  TestList
    [ safeHead ([] :: [Int]) ~?= Nothing,
      safeHead [3, 7, 9] ~?= Just 3
    ]

testSafeTail =
  TestList
    [ safeTail [3, 7, 9] ~?= Just [7, 9]
    ]

testYourGcd =
  TestList
    []

testIntListFilter =
  TestList
    [ intListFilter (< 4) [4, 2, 1, 1, 8, 3] ~?= [2, 1, 1, 3],
      intListFilter odd [1 .. 10] ~?= [1, 3, 5, 7, 9]
    ]

testHanoi =
  TestList
    [ hanoi 0 "a" "b" "c" ~?= [],
      hanoi 1 "a" "b" "c" ~?= [("a", "b")],
      hanoi 2 "a" "b" "c" ~?= [("a", "c"), ("a", "b"), ("c", "b")]
    ]

main :: IO Counts
main = runTestTT allTests
