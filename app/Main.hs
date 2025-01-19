{-# LANGUAGE DeriveAnyClass #-}

import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as B
import Data.Aeson (eitherDecode)
import TuringData


loadMachineFromFile :: FilePath -> IO (Either String Turing)
loadMachineFromFile filePath = do
  jsonData <- B.readFile filePath
  return (eitherDecode jsonData)

main :: IO ()
main = do
  args <- getArgs
  case args of
    (filePath:_) -> do
      result <- loadMachineFromFile filePath
      case result of
        Left err -> putStrLn $ err
        Right turing -> putStrLn $ show turing
    [] -> putStrLn "Argument Error!"
