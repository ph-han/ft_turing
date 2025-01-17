{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs)
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T

-- Address 타입 정의 (중첩 객체)
data Address = Address
  { street  :: T.Text
  , city    :: T.Text
  , zipcode :: T.Text
  } deriving (Show)


-- JSON을 매핑할 Haskell 데이터 타입 정의
data Person = Person
  { name   :: T.Text
  , age    :: Int
  , address :: Address
  } deriving (Show)

instance FromJSON Address where
    parseJSON = withObject "Address" $ \v -> Address
        <$> v .: "street"
        <*> v .: "city"
        <*> v .: "zipcode"

-- FromJSON 인스턴스 구현 (자동 파싱)
instance FromJSON Person where
  parseJSON = withObject "Person" $ \v -> Person
    <$> v .: "name"
    <*> v .: "age"
    <*> v .: "address"

loadPerson :: FilePath -> IO (Either String Person)
loadPerson filePath = do
  jsonData <- B.readFile filePath
  let parsedData = eitherDecode jsonData  -- JSON을 파싱하여 Either 타입 반환
  return parsedData

main :: IO ()
main = do
    args <- getArgs
    case args of
        (filePath:_) -> do
            result <- loadPerson filePath
            case result of
                Left err    -> putStrLn $ "JSON 파싱 오류: " ++ err
                Right person -> print person  -- 성공하면 출력
        [] -> putStrLn "사용법: 프로그램 실행 시 JSON 파일 경로를 입력하세요."
