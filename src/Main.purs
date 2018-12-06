module Main where

import Data.Foldable (foldM)
import Data.List (List(Nil), (:))
import Data.Int (fromString)
import Prelude

import Data.String (splitAt)
import Data.String.CodeUnits (charAt)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Either (Either(Left, Right))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)

newtype ListError = ListError String

instance listErrorShowInstance :: Show ListError where
  show (ListError s) = s  


main :: Effect Unit
main = do
  log (show (calculateValue ("+1" : "+2" : "+3" : Nil)))


reduceFunction :: Int -> Char -> Int -> Either ListError Int
reduceFunction acc '+' number = Right (acc + number)
reduceFunction acc '-' number = Right (acc - number)
reduceFunction _ unknownOperator _ = Left (ListError "Unknown Operator")

breakValue :: String -> Maybe (Tuple Char Int)
breakValue s = do
  char <- charAt 0 s
  let result = splitAt 1 s
  number <- fromString result.after
  pure (Tuple char number)

r :: Int -> String -> Either ListError Int
r n s =
  case (breakValue s) of
    Just (Tuple character number) -> reduceFunction n character number
    Nothing -> Left (ListError "No way")

calculateValue :: List String -> Either ListError Int
calculateValue l = foldM r 0 l