

module TuringMachine (runMachine, buildTape) where

import TuringData
import qualified Data.Text as T
import Data.List (find)

data Tape = Tape
  { tape        :: String
  , tapeHead    :: Int
  , state       :: (String, Char)
  , tapeAction  :: (String, Char, Bool)
  }

instance Show Tape where
  show (Tape t idx s ta) =
    let (ns, w, a) = ta
    in "[" ++ take idx t ++ "<" ++ snd s : ">" ++ drop (idx + 1) t ++ "] (" ++
    fst s ++ ", " ++ snd s : ") -> (" ++ ns ++ ", " ++ w : ", " ++ (\x -> if x then "RIGHT" else "LEFT") (a) ++ ")"

findAction :: (String, Char) -> [(T.Text, [TransitionFunc])] -> (String, Char, Bool)
findAction (st, sy) ts =
  case lookup (T.pack st) ts of
    Nothing -> error "No matching state found in transitions"
    Just funcs -> case find (\t -> T.unpack (readval t) == [sy]) funcs of
      Just tFunc -> (T.unpack $ to_state tFunc, head $ T.unpack $ writeval tFunc, T.unpack (action tFunc) == "RIGHT")
      Nothing -> error "No matching transition function found"

buildTape :: Turing -> String -> Tape
buildTape turing str =
  Tape (str ++ charGen (length str) (getBlank turing)) 0 (T.unpack (initial turing), head str) (findAction (T.unpack (initial turing), head str) (transitions turing))

replaceList :: [a] -> a -> Int -> [a]
replaceList src val idx
    | length src < idx || idx < 0 = src
    | otherwise = (take idx src) ++ val : (drop (idx + 1) src)

moveTape :: Turing -> Tape -> Either Tape String
moveTape tur cur =
  let (ns, w, a) = tapeAction cur
      nextTape = replaceList (tape cur) w (tapeHead cur)
      nextIdx = if a then tapeHead cur + 1 else tapeHead cur - 1
  in if nextIdx < 0
       then Right "Error: Tape head moved out of bounds"
       else let nextState = (ns, head (drop nextIdx nextTape))
                nextAction = findAction nextState (transitions tur)
            in Left (Tape nextTape nextIdx nextState nextAction)


runMachine ::Turing -> Either Tape String -> IO ()
runMachine _ (Right errMsg) = putStrLn errMsg
runMachine tu (Left tp) = do
    print tp
    let (ns, _, _) = tapeAction tp
    if (isSubsetOf [ns] (map T.unpack (finals tu))) then return ()
    else runMachine tu (moveTape tu tp)

