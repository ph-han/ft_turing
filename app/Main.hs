{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Data.Aeson (FromJSON, ToJSON, eitherDecode)
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

-- 데이터 타입 정의
data TransitionFunc = TransitionFunc
  { read      :: T.Text
  , to_state  :: T.Text
  , write     :: T.Text
  , action    :: T.Text
  } deriving (Show, Generic)

data Turing = Turing
  { name        :: T.Text
  , alphabet    :: [T.Text]
  , blank      :: T.Text
  , states      :: [T.Text]
  , initial     :: T.Text
  , finals      :: [T.Text]
  , transitions :: HM.HashMap T.Text [TransitionFunc]
  } deriving (Show, Generic)

-- FromJSON, ToJSON 자동 파생
instance FromJSON TransitionFunc
instance ToJSON TransitionFunc

instance FromJSON Turing
instance ToJSON Turing

-- JSON 파일 읽기 및 파싱
loadMachineFromFile :: FilePath -> IO (Either String Turing)
loadMachineFromFile filePath = do
  jsonData <- B.readFile filePath
  return $ eitherDecode jsonData

-- 메인 함수
main :: IO ()
main = do
  let filePath = "./machine/unary_sub.json"
  result <- loadMachineFromFile filePath
  case result of
    Left err     -> putStrLn $ "Error: " ++ err
    Right turing -> print turing
