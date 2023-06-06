module Main where

import Prelude

import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Core (caseJson, Json(..), fromObject, fromArray, stringifyWithIndent)
import Data.Either as E
import Data.Lazy (force)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), fst)
import Data.Array (zip)
import Effect (Effect)
import Effect.Console (log)
import JsonPos as J

testIn :: String
testIn =
  """[{$project :
  {_id : true,
   y : {$add: ['$x', 2]}}}]"""

testSrcLoc :: String
testSrcLoc = "[{$project: {_id: true, y: {$add: [{originalExpr: '$x', sourceLocId: 5}, {$const: 1, sourceLocId: 6}], sourceLocId: 4}}}]"

main :: Effect Unit
main = do
  let toks = fromMaybe [] (J.tokenize testIn)
  log $ "input: " <> show testIn
  log "toks"
  log $ show $ toks
  log "parsed json"
  let posed = fst $ fromMaybe (Tuple { json: J.JNull, pos: { col: 0, row: 0 } } []) $ force J.parseJson toks
  log $ stringifyWithIndent 4 $ J.toJson posed
