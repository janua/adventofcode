module Main where

import Prelude

import Data.Traversable (sequence, traverse_)
import DayOne (solveDayOne)
import Effect (Effect)
import Effect.Console (log)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.HTMLTextAreaElement (value, fromElement)
import Web.HTML.Window (document)
  

main :: Effect Unit
main = do
  browserWindow <- window
  windowDocument <- document browserWindow
  let 
    parentNode = toNonElementParentNode windowDocument
  maybeInputText <- getElementById "input-text" parentNode
  let maybeText = maybeInputText >>= fromElement
  maybeThing <- sequence (map value maybeText)
  let maybeAnswer = map solveDayOne maybeThing
  let maybeOutput = map show maybeAnswer
  traverse_ log maybeOutput