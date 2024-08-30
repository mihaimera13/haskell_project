module Args where

import Data.List
import Result
import Text.Read (readMaybe)

data Args = Args
  { argImageConfigFile :: Maybe String, -- ^ Given with the `-imageConfigFile` argument
    argSceneFile :: Maybe String, -- ^ Given with the `-sceneFile` argument
    argOutFile :: Maybe String, -- ^ Given with the `-outFile` argument
    argNrSamples :: Maybe Int -- ^ Given with the `-imageNrSamples` argument
  }
  deriving (Eq, Show)

data ParseArgsError = InvalidArgs deriving (Eq, Show)

type ArgMap = [(String, String)]

-- >>> toArgMap ["-x", "y"]
-- Success [("x","y")]
--
-- >>> toArgMap ["-x", "y", "-a", "b"]
-- Success [("x","y"),("a","b")]
--
-- >>> toArgMap ["x", "y"]
-- Error InvalidArgs
--
-- >>> toArgMap ["-x", "y", "-z"]
-- Error InvalidArgs
toArgMap :: [String] -> Result ParseArgsError ArgMap
toArgMap list = solveProblem list
                where solveProblem list | (odd (length list)) = Error InvalidArgs
                                        | otherwise = makeTuples list []
                                          where makeTuples [] acc = Success (reverse acc)
                                                makeTuples (x1:x2:xs) acc = 
                                                    if head x1 == '-' then (makeTuples xs ((tail x1,x2):acc)) else Error InvalidArgs

-- >>> getArg "key" [("key", "value")]
-- Just "value"
getArg :: String -> ArgMap -> Maybe String
getArg value list = solveProblem filtered
    where
        filtered = ((map (\(x,y) -> y)).(filter (\(x,y) -> x == value))) list
        solveProblem [] = Nothing
        solveProblem (x:_) = Just x

-- >>> readArg "name" [("name", "1")] :: Maybe Int
-- Just 1
--
-- >>> readArg "name" [("name", "one")] :: Maybe Int
-- Nothing
--
-- >>> readArg "number" [("name", "1")] :: Maybe Int
-- Nothing
readArg :: (Read a) => String -> ArgMap -> Maybe a
readArg value list = 
    case getArg value list of
        Nothing -> Nothing
        Just x -> case readMaybe x of
                    Nothing -> Nothing
                    Just y -> Just y

-- >>> procArgs ["-imageNrSamples", "200", "-outFile", "image.bmp"]
-- Success (Args {argImageConfigFile = Nothing, argSceneFile = Nothing, argOutFile = Just "image.bmp", argNrSamples = Just 200})
procArgs :: [String] -> Result ParseArgsError Args
procArgs list = 
    case toArgMap list of
        Error InvalidArgs -> Error InvalidArgs
        Success lst -> Success ( Args {argImageConfigFile = (getArg "imageConfigFile" lst),
                                      argSceneFile = (getArg "sceneFile" lst),
                                      argOutFile = (getArg "outFile" lst),
                                      argNrSamples = (readArg "imageNrSamples" lst)}) 
        
