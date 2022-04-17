module Main where

import Core ( parseDisplayTodos, parseDisplayArgDate )

import System.Directory
import System.Environment (getArgs)
import Data.Time ( getCurrentTime )
import Data.Time.Format.ISO8601 (iso8601Show)

main :: IO ()
main = do
  -- createDirectoryIfMissing False getHomeDirectory
  path <- (++"/.config/motonad") <$> getHomeDirectory
  createDirectoryIfMissing True path


  getArgs >>= parseArgs path

monadrc :: IO (Maybe String)
monadrc = do
  file   <- (++"/.config/motonad/motonadrc") <$> getHomeDirectory
  exists <- doesFileExist file
  sequenceA $ if exists
    then Just $ readFile file
    else Nothing


parseArgs :: FilePath -> [String] -> IO ()
parseArgs path [x:xs, y]
  | x == '+' || x == '-' = do
      time <- getCurrentTime
      case parseDisplayArgDate time (x:xs) of
        Left  err   -> putStrLn err
        Right time' ->
          appendFile
            (path++"/motonadrc")
            ("{: due [" ++ iso8601Show time' ++ "]\n" ++ y ++ "\n")

parseArgs path _ = do
  file <- monadrc
  case file of
    Nothing       -> putStrLn $ "Failure: no motonadrc file located in " ++ path
    Just contents ->
      case parseDisplayTodos "monadrc" contents of
        Left  err -> putStrLn $ "Error reading motonad todo file:\n"++err
        Right res -> putStrLn $
          if null res
            then "You have no tasks to complete."
            else res
