  import Chatterbot
  import Test.HUnit

  reflectTest =
    test [
      reflect ["i", "will", "never", "see", "my", "reflection", "in", "your", "eyes"]
          ~?= ["you", "will", "never", "see", "your", "reflection", "in", "my", "eyes"]
    ]

  transformations = [(words "I hate *", words "Why do you hate * ?")]

  testMember v options = test $ assertBool message $ v `elem` options
    where message = show (v) ++ " not part of: " ++ show options

  rulesApplyTest =
    test [
      rulesApply transformations (words "I hate my mother")
        ~?= (words "Why do you hate your mother ?"),
      rulesApply transformations (words "ARGH!") `testMember` [(words "ARGH!"), (words "")]
    ]

  reduceTest =
    test [
      (reduce.words) "can you please tell me what Haskell is" ~?= words "what is Haskell"
    ]

  reduceTest1 =
    test [
      (reduce.words) "i am very very tired" ~?= words "i am tired"
    ]


  substituteTest =
    test [
      substitute 'x' "3*cos(x) + 4 - x" "5.37" ~?= "3*cos(5.37) + 4 - 5.37"
    ]

  matchTest =
    test [
      match 'x' "2*x+3" "2*7+3" ~?= Just "7",
      match '*' "frodo" "gandalf" ~?= Nothing,
      match 2 [1,3..5] [1,3..5] ~?= Just [],
      match '*' "* and *" "you and me" ~?= Just "you",
      match 'x' "2*x+3+x" "2*7+3" ~?= Nothing,
      match '*' "*do" "bdo" ~?= Just "b",
      match '*' "*do" "dobedo" ~?= Just "dobe",
      match '*' "*do" "bedobe" ~?= Nothing,
      match '*' "" "" ~?= Just [],
      match '*' "abba" "" ~?= Nothing,
      match '*' "" "abba" ~?= Nothing,
      match '*' "a" "a" ~?= Just [],
      match '*' "*" "a" ~?= Just "a",
      match '*' "*" "abba" ~?= Just "abba",
      match '*' "*X*" "aXb" ~?= Just "a",
      match '*' "*X*" "aaXbb" ~?= Just "aa"
    ]

  frenchPresentation = ("My name is *", "Je m'appelle *")

  transformationApplyTest =
    test [
      transformationApply '*' id "My name is Zacharias" frenchPresentation
        ~?= Just "Je m'appelle Zacharias",
      transformationApply '*' id "My shoe size is 45" frenchPresentation
        ~?= Nothing
    ]

  swedishPresentation = ("My name is *", "Mitt namn är *")
  presentations = [frenchPresentation, swedishPresentation]

  transformationsApplyTest =
    test [
      transformationsApply '*' id presentations "My name is Zacharias"
        ~?= Just "Je m'appelle Zacharias",
      transformationsApply '*' id (reverse presentations) "My name is Zacharias"
        ~?= Just "Mitt namn är Zacharias",
      transformationsApply '*' id (reverse presentations) "My shoe size is 45"
        ~?= Nothing
    ]



  main = runTestTT $
    test [
      "substitute" ~: Main.substituteTest,
      "match" ~: Main.matchTest,
      "transformationApply" ~: transformationApplyTest,
      "transformationsApply" ~: transformationsApplyTest,
      "reflect" ~: reflectTest,
      "rulesApply" ~: rulesApplyTest,
      "reduceTest" ~: reduceTest,
      "reduceTest1" ~: reduceTest1
    ]
