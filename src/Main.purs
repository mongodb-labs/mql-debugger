module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Data.Maybe (fromMaybe)
import Data.Lazy (force)
import Data.Tuple (Tuple(..), fst)
import JsonPos as J


testIn :: String
testIn = "[{$match : {x : {$gt: 1}}}, \n\
\  {$project : {y : {$add: ['$x', 2]}}}]"

main :: Effect Unit
main = do
  let toks = fromMaybe [] (J.tokenize testIn)
  log $ "input: " <> show testIn
  log "toks"
  log $ show $ toks
  log "parsed json"
  log $ show $ fst $ fromMaybe (Tuple { json : J.JNull, pos : { col : 0, row : 0 } } []) $ force J.parseJson toks
