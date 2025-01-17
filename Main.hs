{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T

-- JSON을 매핑할 Haskell 데이터 타입 정의
data Person = Person
  { name   :: T.Text
  , age    :: Int
  , skills :: [T.Text]
  } deriving (Show)

-- FromJSON 인스턴스 구현 (자동 파싱)
instance FromJSON Person where
  parseJSON = withObject "Person" $ \v -> Person
    <$> v .: "name"
    <*> v .: "age"
    <*> v .: "skills"

loadPerson :: FilePath -> IO (Either String Person)
loadPerson filePath = do
  jsonData <- B.readFile filePath
  let parsedData = eitherDecode jsonData  -- JSON을 파싱하여 Either 타입 반환
  return parsedData

main :: IO ()
main = do
  result <- loadPerson "data.json"
  case result of
    Left err    -> putStrLn $ "JSON 파싱 오류: " ++ err
    Right person -> print person  -- 성공하면 출력
