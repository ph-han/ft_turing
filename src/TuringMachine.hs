

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

instance Eq Tape where
  (Tape _ _ s1 a1) == (Tape _ _ s2 a2) =
    s1 == s2 && a1 == a2

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
      tHead = if tapeHead cur == 0 && not a then 1 else tapeHead cur
      nextTape =
        (if tapeHead cur == 0 && not a then charGen 1 (getBlank tur) else "")
        ++ replaceList (tape cur) w (tapeHead cur) ++
        (if tapeHead cur == length (tape cur) - 1 && a then charGen 1 (getBlank tur) else "")
      nextIdx = if a then tHead + 1 else tHead - 1
      nextState = (ns, head (drop nextIdx nextTape))
      nextAction = findAction nextState (transitions tur)
      newTape = Tape nextTape nextIdx nextState nextAction
  in if newTape == cur && (nextIdx == 0 || nextIdx == length nextTape - 1)
       then Right "Error: Tape status recursion"
       else Left newTape


runMachine ::Turing -> Either Tape String -> IO ()
runMachine _ (Right errMsg) = putStrLn errMsg
runMachine tu (Left tp) = do
    print tp
    let (ns, _, _) = tapeAction tp
    if (isSubsetOf [ns] (map T.unpack (finals tu))) then return ()
    else runMachine tu (moveTape tu tp)


