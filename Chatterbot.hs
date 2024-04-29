module Chatterbot where
  import Utilities
  import System.Random
  import Data.Char
  import Data.Maybe (fromMaybe)

chatterbot :: String -> [(String, [String])] -> IO ()
chatterbot botName botRules = do
  putStrLn ("\n\nHi! I am " ++ botName ++ ". How are you?")
  botloop
  where
    brain = rulesCompile botRules
    botloop = do
      putStr "\n: "
      question <- getLine
      answer <- stateOfMind brain
      putStrLn (botName ++ ": " ++ (present . answer . prepare) question)
      if (not . endOfDialog) question then botloop else return ()

--------------------------------------------------------

type Phrase = [String]
type PhrasePair = (Phrase, Phrase)
type BotBrain = [(Phrase, [Phrase])]


--------------------------------------------------------

stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
{- TO BE WRITTEN -}
stateOfMind brain = do
  let (_, responses) = brain !! randIndex
  let response = responses !! randIndex
  return (rulesApply [(["*"], response)])

rulesApply :: [PhrasePair] -> Phrase -> Phrase
{- TO BE WRITTEN -}
rulesApply pairs phrase =
  case transformationsApply '*' id pairs phrase of
    Just result -> result
    Nothing -> phrase

reflect :: Phrase -> Phrase
{- TO BE WRITTEN -}
reflect = map (\word -> fromMaybe word (lookup word reflections))

reflections :: [(String, String)]
reflections =
  [ ("am",     "are"),
    ("was",    "were"),
    ("i",      "you"),
    ("i'm",    "you are"),
    ("i'd",    "you would"),
    ("i've",   "you have"),
    ("i'll",   "you will"),
    ("my",     "your"),
    ("me",     "you"),
    ("are",    "am"),
    ("you're", "i am"),
    ("you've", "i have"),
    ("you'll", "i will"),
    ("your",   "my"),
    ("yours",  "mine"),
    ("you",    "me")
  ]


---------------------------------------------------------------------------------

endOfDialog :: String -> Bool
endOfDialog = (=="quit") . map toLower

present :: Phrase -> String
present = unwords

prepare :: String -> Phrase
prepare = reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|") 

rulesCompile :: [(String, [String])] -> BotBrain
{- TO BE WRITTEN -}
rulesCompile = map (\(pattern, responses) -> (words pattern, map words responses))


--------------------------------------


reductions :: [PhrasePair]
reductions = (map.map2) (words, words)
  [ ( "please *", "*" ),
    ( "can you *", "*" ),
    ( "could you *", "*" ),
    ( "tell me if you are *", "are you *" ),
    ( "tell me who * is", "who is *" ),
    ( "tell me what * is", "what is *" ),
    ( "do you know who * is", "who is *" ),
    ( "do you know what * is", "what is *" ),
    ( "are you very *", "are you *" ),
    ( "i am very *", "i am *" ),
    ( "hi *", "hello *")
  ]

reduce :: Phrase -> Phrase
reduce = reductionsApply reductions

reductionsApply :: [PhrasePair] -> Phrase -> Phrase
{- TO BE WRITTEN -}
reductionsApply [] phrase = phrase
reductionsApply ((pattern, substitution):rest) phrase =
  case match '*' pattern phrase of
    Just matched -> substitute '*' substitution matched ++ reductionsApply rest phrase
    Nothing -> reductionsApply rest phrase


-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute _ _ _ = []
{- TO BE WRITTEN -}
substitute wildcard p = foldr (\x acc -> if x == wildcard then p else x : acc) []


-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ _ _ = Nothing
{- TO BE WRITTEN -}
match wildcard pattern list =
  case dropWhile (/= wildcard) (zipWith (\x y -> if x == y then Just y else Nothing) pattern list) of
    [] -> Nothing
    (_, rest) -> Just (takeWhile (/= wildcard) rest)


-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch (wc:ps) (x:xs)
{- TO BE WRITTEN -}
  | wc == x = Just xs  -- If the first element of the pattern is a wildcard and matches the corresponding element in the list, we consume that element and return the rest of the list
  | otherwise = Nothing -- If the wildcard doesn't match, we return Nothing

longerWildcardMatch (wc:ps) (x:xs)
{- TO BE WRITTEN -}
  | wc == x = case match wc ps xs of -- If the first element of the pattern is a wildcard and matches the corresponding element in the list, we use match to find the sublist that matches the remaining pattern
    Just r -> Just r
    Nothing -> longerWildcardMatch (wc:ps) xs -- If there's no match, we continue searching for a match with the remaining list
  | otherwise = Nothing -- If the wildcard doesn't match, we return Nothing



-- Test cases --------------------

testPattern =  "a=*;"
testSubstitutions = "32"
testString = "a=32;"

substituteTest = substitute '*' testPattern testSubstitutions
substituteCheck = substituteTest == testString

matchTest = match '*' testPattern testString
matchCheck = matchTest == Just testSubstitutions



-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------

-- Applying a single pattern
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
{- TO BE WRITTEN -}
transformationApply wildcard transformFn input (pattern, substitution) =
  case match wildcard pattern input of
    Just matchedSublist -> Just (transformFn matchedSublist ++ drop (length pattern) input)
    Nothing -> Nothing

-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
{- TO BE WRITTEN -}
transformationsApply _ _ [] _ = Nothing
transformationsApply wildcard transformFn ((pattern, substitution):patterns) input =
  case transformationApply wildcard transformFn input (pattern, substitution) of
    Just result -> Just result
    Nothing -> transformationsApply wildcard transformFn patterns input

