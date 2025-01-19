{-# LANGUAGE DeriveAnyClass #-}

import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as B
import Data.Aeson (eitherDecode)
import TuringData

loadMachineFromFile :: FilePath -> IO (Either String Turing)
loadMachineFromFile filePath = do
  jsonData <- B.readFile filePath
  return (eitherDecode jsonData)

printHelp :: IO ()
printHelp =
  putStrLn ("usage: ft_turing [-h] jsonfile input\n\n" ++
           "positional arguments:\n" ++
           " jsonfile\t\tjson description of the machine\n" ++
           " input\t\t\tinput of the machine\n\n" ++
           "optional arguments:\n" ++
           " -h, --help\t\tshow this help message and exit")

-- showCurrState :: String -> IO()
-- showCurrState tape =

runMachine :: Turing -> String -> IO ()
runMachine turing inputStr =
  putStrLn inputStr

main :: IO ()
main = do
  args <- getArgs
  case args of
    (cmd:_) | isHelp cmd -> printHelp
    [filePath, inputStr] -> do
      result <- loadMachineFromFile filePath
      case result of
        Left err -> putStrLn $ "[Error]: " ++ err
        Right turing -> do
          print turing
          runMachine turing inputStr
    _ -> printHelp
    where
      isHelp cmd = cmd == "-h" || cmd == "--help"
