{-# LANGUAGE DeriveAnyClass #-}

import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as B
import Data.Aeson (eitherDecode)
import TuringData
import TuringMachine

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


main :: IO ()
main = do
  args <- getArgs
  case args of
    (cmd:_) | isHelp cmd -> printHelp
    [filePath, inputStr] -> do
      eitherTuring <- loadMachineFromFile filePath
      let result = checkInput eitherTuring inputStr
      case result of
        Left err -> putStrLn err
        Right turing -> do
          print turing
          runMachine turing (Left (buildTape turing inputStr))
    _ -> printHelp
    where
      isHelp cmd = cmd == "-h" || cmd == "--help"
