{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}


module TuringData (Turing(..), TransitionFunc(..), checkInput, charGen, getBlank, isSubsetOf) where  -- ✅ 모듈 선언
import Data.Aeson (parseJSON, FromJSON, withObject, (.:))
import GHC.Generics (Generic)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Data.List (intercalate, findIndex)

data TransitionFunc = TransitionFunc
  { readval      :: T.Text
  , to_state  :: T.Text
  , writeval     :: T.Text
  , action    :: T.Text
  } deriving (Generic)


-- ✅ TransitionFunc의 Show 인스턴스를 수정하여 원하는 형식 적용
instance Show TransitionFunc where
  show (TransitionFunc r to w act) =
    T.unpack r ++ ") -> (" ++ T.unpack to ++ ", " ++ T.unpack w ++ ", " ++ T.unpack act ++ ")"

data Turing = Turing
  { name        :: T.Text
  , alphabet    :: [T.Text]
  , blank       :: T.Text
  , states      :: [T.Text]
  , initial     :: T.Text
  , finals      :: [T.Text]
  , transitions :: [(T.Text, [TransitionFunc])]
  } deriving (Generic)

machineTitleGen :: (Int -> Char -> String) -> String -> String
machineTitleGen f title =
  let width = 80  -- 전체 가로 길이
      border = f width '*'  -- 상/하 테두리
      padding = '*' : (f (width - 2) ' ') ++ "*"  -- 공백 라인
      titlePadding = (width - length title) `div` 2  -- 중앙 정렬을 위한 공백 계산
      titleLine = "*" ++ f (titlePadding - 1) ' ' ++ title ++ f (width - titlePadding - length title - 1) ' ' ++ "*"
  in unlines [border, padding, titleLine, padding, border]  -- 줄바꿈을 포함한 전체 문자열

getBlank :: Turing -> Char
getBlank turing = head $ T.unpack $ blank turing

charGen :: Int -> Char -> String
charGen 0 _ = ""
charGen n c = c : charGen (n - 1) c

-- ✅ Turing의 Show 인스턴스를 수정하여 (state, read) -> (to_state, write, action) 형식 적용
instance Show Turing where
  show (Turing name alphabet _ states initial finals transitions) =
    machineTitleGen charGen (T.unpack name) ++
    "Alphabet : [ " ++ intercalate ", " (map T.unpack alphabet) ++ " ]\n" ++
    "States : [ " ++ intercalate ", " (map T.unpack states) ++ " ]\n" ++
    "Initial : " ++ T.unpack initial ++ "\n" ++
    "Final States : [ " ++ intercalate ", " (map T.unpack finals) ++ " ]\n" ++
    concat (map showTransition transitions) ++
    charGen 80 '*'
    where
      -- ✅ (state, read) -> (to_state, write, action) 형식 적용
      showTransition (state, funcs) = unlines (map (\t -> "(" ++ T.unpack state ++ ", " ++ show t) funcs)

instance FromJSON TransitionFunc where
  parseJSON = withObject "TransitionFunc" $ \v -> do
    readval     <- v .: "read"
    to_state    <- v .: "to_state"
    writeval    <- v .: "write"
    action      <- v .: "action"
    return $ TransitionFunc readval to_state writeval action

-- `FromJSON`을 직접 구현하여 `states` 순서대로 `transitions`을 정렬
instance FromJSON Turing where
  parseJSON = withObject "Turing" $ \v -> do
    name        <- v .: "name"
    alphabet    <- v .: "alphabet"
    blank       <- v .: "blank"
    states      <- v .: "states"
    initial     <- v .: "initial"
    finals      <- v .: "finals"
    transitionsObj <- v .: "transitions"  -- JSON의 객체(HashMap)
    let transitionsList = HM.toList transitionsObj  -- HashMap → List 변환
        -- ✅ states 순서대로 transitions 정렬
        sortedTransitions = [(s, t) | s <- states, Just t <- [lookup s transitionsList]]
    return $ Turing name alphabet blank states initial finals sortedTransitions

checkInput :: Either String Turing -> String -> Either String Turing
checkInput val s = case val of
  Left err -> Left err
  Right turing -> validateTuring turing s

validateTuring :: Turing -> String -> Either String Turing
validateTuring turing s =
  let alphabetStr = T.unpack (mconcat $ alphabet turing)
      blankStr = T.unpack $ blank turing
      stateList = map T.unpack $ states turing
      finalStates = map T.unpack $ finals turing
      initialState = T.unpack $ initial turing

      checks =
        [ (sum (map T.length (alphabet turing)) == length (alphabet turing),
            "Error: Alphabet contains multi-character symbols")
        , (not (isSubsetOf blankStr s),
            "Error: Blank appears in input")
        , (isSubsetOf s alphabetStr,
            "Error: Unknown character in input")
        , (isSubsetOf blankStr alphabetStr,
            "Error: Blank does not match with alphabet")
        , (isSubsetOf [initialState] stateList,
            "Error: Initial state is not in state list")
        , (isSubsetOf finalStates stateList,
            "Error: Final states are not in state list")
        ]
  in case lookup False checks of
      Just errMsg -> Left errMsg
      Nothing     -> Right turing

isSubsetOf :: Ord a => [a] -> [a] -> Bool
isSubsetOf s1 s2 = case s1 of
  [] -> True
  x:xs -> (\m -> case m of Just _ -> isSubsetOf xs s2; Nothing -> False) (findIndex (==x) s2)
