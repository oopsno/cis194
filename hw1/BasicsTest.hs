import Basics
import Test.HUnit

allTests =
  TestList
    [ "tellUsAboutYourself" ~: testTellUsAboutYourself,
      "fullName" ~: testFullName,
      "lastCommaFirst" ~: testLastCommaFirst,
      "initials" ~: testInitials,
      "listExercises" ~: testListExercises,
      "addIntPairs" ~: testAddIntPairs
    ]

testTellUsAboutYourself =
  TestList
    [ t "firstName is undefined" firstName,
      t "lastName is undefined" lastName,
      t "favoriteNumber is undefined" favoriteNumber,
      t "githubUsername is undefined" githubUsername
    ]
  where
    t failureMessage value =
      test . assertBool failureMessage $ value `seq` True

testFullName =
  TestList
    [ fullName "Haskell" "Curry" ~?= "Haskell Curry",
      fullName "" "NoFirstName" ~?= " NoFirstName",
      fullName "NoLastName" "" ~?= "NoLastName ",
      fullName "" "" ~?= " "
    ]

testLastCommaFirst =
  TestList
    [ lastCommaFirst "George" "Washington" ~?= "Washington, George"
    ]

testInitials =
  TestList
    [ initials "Ada" "Lovelace" ~?= "AL",
      initials "alan" "turing" ~?= "AT"
    ]

testListExercises =
  TestList
    [ test . assertBool "failure: oddsBetweenZeroAndTen" $
        all (`elem` oddsBetweenZeroAndTen) [1, 3, 5, 7, 9],
      test . assertBool "failure: someOddNumbers" $
        all odd someOddNumbers,
      multiplesOfThreeUpTo20 ~?= [3, 6, 9, 12, 15, 18]
    ]

testAddIntPairs =
  TestList
    [ addIntPairs (1, 2) (3, 4) ~?= (4, 6),
      addIntPairs (-1, 3) (2, 0) ~?= (1, 3)
    ]

main :: IO Counts
main = runTestTT allTests
