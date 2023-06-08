module Parser where

import Prelude

import Data.Argonaut.Core (caseJson, Json(..), fromObject, fromArray, stringifyWithIndent, toArray, toObject, jsonSingletonObject, jsonNull)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (zip)
import Data.Either as E
import Data.Lazy (force)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), fst)
-- import Effect (Effect)
-- import Effect.Console (log)
import Foreign.Object (Object)
import Foreign.Object as Object
import JsonPos as J

-- testIn :: String
-- testIn =
--   """[{$project :
--   {_id : true,
--    y : {$add: ['$x', 1]}}}]"""

-- testSrcLoc :: String
-- testSrcLoc = "[{\"$project\": {\"_id\": true, \"y\": {\"$add\": [{\"originalExpr\": \"$x\", \"sourceLocId\": 5}, {\"$const\": 1, \"sourceLocId\": 6}], \"sourceLocId\": 4}}}]"

-- main :: Effect Unit
-- main = do
--   let toks = fromMaybe [] (J.tokenize testIn)
--   log $ "input: " <> show testIn
--   log "toks"
--   log $ show $ toks
--   log "parsed json"
--   let posed = J.toJson $ fst $ fromMaybe (Tuple { json: J.JNull, pos: { col: 0, row: 0 } } []) $ force J.parseJson toks
--   -- log $ stringifyWithIndent 4 $ J.toJson posed
--   case jsonParser testSrcLoc of
--     E.Left e -> log $ "failed parsing: " <> e
--     E.Right j -> log $ stringifyWithIndent 4 $ mergeAwithP j posed

tokenize :: String -> Array { pos :: J.Pos, tok :: J.Token }
tokenize input = fromMaybe [] $ J.tokenize input

parseWithPos :: String -> Json
parseWithPos input = J.toJson $ fst $ fromMaybe (Tuple { json: J.JNull, pos: { col: 0, row: 0 } } []) $ ((J.tokenize input) >>= (force J.parseJson))

mergeAwithP :: Json -> Json -> Json
mergeAwithP a p = caseJson
  (const a)
  (const a)
  (const a)
  (const a)
  (\arr -> fromArray $ fromMaybe arr ((\p -> map (\(Tuple a' p') -> mergeAwithP a' p') $ zip arr p) <$> (toArray p)))
  (\obj -> fromObject $ fromMaybe obj $ (mergeObj obj) <$> (toObject p))
  a

mergeObj :: Object Json -> Object Json -> Object Json
mergeObj a p = case Object.lookup "__srcPos" p of
  Just pos -> Object.union (mergeObj' a p) $ Object.fromFoldable [ Tuple "__srcPos" pos ]
  Nothing -> mergeObj' a p
  where
  mergeObj' = Object.unionWith (\a p -> mergeAwithP a p)
