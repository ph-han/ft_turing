
module TuringMachine (buildTape, nextTape) where

import TuringData
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Data.List (find)

data Tape = Tape
  { tape      :: String
  , tapeHead      :: Int
  , state     :: (String, Char)
  , tapeAction    :: (String, Char, Bool)
  } deriving (Show)

findAction :: (String, Char) -> [(T.Text, [TransitionFunc])] -> (String, Char, Bool)
findAction (state, symbol) transitions =
  case lookup (T.pack state) transitions of
    Nothing -> error "No matching state found in transitions"
    Just funcs -> case find (\t -> T.unpack (readval t) == [symbol]) funcs of
      Just tFunc -> (T.unpack $ to_state tFunc, head $ T.unpack $ writeval tFunc, T.unpack (action tFunc) == "RIGHT")
      Nothing -> error "No matching transition function found"

buildTape :: Turing -> String -> Tape
buildTape turing str =
  Tape str 0 (T.unpack (initial turing), head str) (findAction (T.unpack (initial turing), head str) (transitions turing))

replaceList :: [a] -> a -> Int -> [a]
replaceList src val idx
    | length src < idx || idx < 0 = src
    | otherwise = (take idx src) ++ val : (drop (idx + 1) src)

nextTape :: Turing -> Tape -> Tape
nextTape tur cur =
  let (ns, w, a) = (tapeAction cur)
      nextTape = replaceList (tape cur) w (tapeHead cur)
      nextIdx = (\x y -> if x then y + 1 else y - 1) a (tapeHead cur)
      nextState = (ns, head (drop nextIdx nextTape))
      nextAction = findAction nextState (transitions tur)
  in Tape nextTape nextIdx nextState nextAction

-- runMachine :: Tape -> IO ()
-- runMachine turing inputStr =
--   print buildTape turing inputStr

-- runMachine :: (Turing -> String -> Tape) -> Turing -> String -> IO ()
