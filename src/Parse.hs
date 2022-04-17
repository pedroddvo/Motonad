{-# LANGUAGE OverloadedStrings #-}
module Parse ( parseTodos, parseArgDate, TodoItem(..) ) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Error

import Data.Void
import Data.Text
import qualified Data.Text as T

import Data.Time.Format.ISO8601
import Data.Time.Clock
import Data.Time.Calendar
import GHC.Unicode (isDigit)

data TodoItem =
  TodoItem { todoTime :: UTCTime
           , todoContent :: Text }
  deriving (Show)

data CustomError = BadTime Text
  deriving (Eq, Show, Ord)

instance ShowErrorComponent CustomError where
  showErrorComponent (BadTime s) = "bad iso8601 time format: " ++ T.unpack s

type Parser = Parsec CustomError Text

badTimeParse :: Parser (Either Text UTCTime) -> Parser UTCTime
badTimeParse p = p >>= \e ->
  case e of
    Left s  -> customFailure (BadTime s)
    Right t -> pure t

maybeEither :: a -> Maybe b -> Either a b
maybeEither _ (Just b) = Right b
maybeEither a Nothing  = Left a

parseDate :: Parser UTCTime
parseDate = between (char '[') (char ']') (badTimeParse parseTime)
  where
    parseTime :: Parser (Either Text UTCTime)
    parseTime = iso8601Parse <$> takeWhile1P (Just "]") (/= ']')
    iso8601Parse t = maybeEither t (iso8601ParseM $ T.unpack t)

parseContent :: Parser String
parseContent = some ((string "\\{" >> return '{') <|> anySingleBut '{') <?> "content"

parseTodo :: Parser TodoItem
parseTodo = toItem <$>
            ((string "{:" *> space1) *> ((,) <$> parseQualifiers <*> parseContent))
  where
    parseQualifiers = (string "due" *> space1) *> parseDate <* some newline

    toItem (date, content) =
      TodoItem { todoTime = date, todoContent = T.pack content }

parseArgDate :: UTCTime -> Parser UTCTime
parseArgDate time =
  choice [ id     <$ char '+'
         , negate <$ char '-' ] >>= argDate time

  where
    argDate :: UTCTime -> (Int -> Int) -> Parser UTCTime
    argDate time f = do
      digits <- takeWhile1P (Just "digits 0-9") isDigit
      f      <- timeDiff (read $ T.unpack digits)
      return $
        UTCTime { utctDay = f (utctDay time)
                , utctDayTime = secondsToDiffTime 0 }

    timeDiff :: Integer -> Parser (Day -> Day)
    timeDiff n =
      choice [ addDays                n <$ char 'd'
             , addGregorianMonthsClip n <$ char 'm'
             , addGregorianYearsClip  n <$ char 'y' ]

parseTodos :: Parser [TodoItem]
parseTodos = parseContent *> many parseTodo <* eof
