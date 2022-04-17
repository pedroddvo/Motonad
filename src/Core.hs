module Core where

import Data.Time.Clock (UTCTime)
import Data.Text ( Text, unpack, pack, strip )

import Text.Megaparsec ( runParser )
import Text.Megaparsec.Error ( errorBundlePretty )

import Parse ( TodoItem(..), parseArgDate, parseTodos )


displayTodos :: [TodoItem] -> String
displayTodos = foldr (\i acc -> displayTodo i ++ "\n" ++ acc) []
  where
    displayTodo i =
      "DUE "
      ++ show (todoTime i)
      ++ ":\n\t"
      ++ (content . unpack . strip) (todoContent i)

    content [] = ""
    content (x:xs)
      | x == '\n' = "\n\t" ++ content xs
      | otherwise = x:content xs

parseDisplayTodos :: String -> String -> Either String String
parseDisplayTodos file input =
  case runParser parseTodos file (pack input) of
    Left  p -> Left  (errorBundlePretty p)
    Right p -> Right (displayTodos p)

parseDisplayArgDate :: UTCTime -> String -> Either String UTCTime
parseDisplayArgDate time input =
  case runParser (parseArgDate time) "" (pack input) of
    Left  p -> Left  (errorBundlePretty p)
    Right p -> Right p
