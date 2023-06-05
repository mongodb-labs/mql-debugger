module JsonPos
  ( Pos
  , Token(..)
  , TokenizeState
  , Tokenizer
  , tokenize
  , parseJson
  , Json(..)
  , JsonWPos(..)
  )
  where

import Prelude
import Control.Monad.State as S
import Data.Lazy (Lazy, defer, force)
import Data.Foldable (foldr)
import Data.Maybe as M
import Data.Either as E
import Data.Tuple as T
import Data.String (CodePoint, toCodePointArray, codePointFromChar, singleton, fromCodePointArray)
import Data.String.Regex as R
import Data.String.Regex.Flags (noFlags)
import Data.Array (cons, uncons, snoc)
import Data.Array as A
import Data.Number as N

import Debug (trace)

data Token = Space
           | Comma
           | Colon
           | ObjStart
           | ObjEnd
           | ArrStart
           | ArrEnd
           | Str String
           | Num Number
           | FieldName String
           | True
           | False
           | Null

instance showToken :: Show Token where
  show Space = "<SPC>"
  show Comma = "\",\""
  show Colon = "\":\""
  show ObjStart = "\"{\""
  show ObjEnd = "\"}\""
  show ArrStart = "\"[\""
  show ArrEnd = "\"]\""
  show (Str s) = s <> " : string"
  show (Num n) = show n <> " : number"
  show (FieldName f) = f <> " : fieldname"
  show False = "False"
  show True = "True"
  show Null = "Null"

type Pos = { row :: Int, col :: Int }

type Tokenizer = { pos :: Pos
                 , buf :: Array CodePoint
                 , quote :: M.Maybe CodePoint
                 , rest :: Array CodePoint
                 }

type TokenizeState = S.State Tokenizer

initTokenizer :: String -> Tokenizer
initTokenizer input = { pos : { row : 0, col : 0}
                      , buf : []
                      , quote : M.Nothing
                      , rest : toCodePointArray input
                      }

tokenize :: String -> M.Maybe (Array { tok :: Token, pos :: Pos })
tokenize input = let res = S.runState tokenize' $ initTokenizer input
                     toks = T.fst res
                     st = T.snd res
                in if st.rest == [] && st.buf == [] && M.isNothing st.quote then M.Just toks else M.Nothing

tokenize' :: TokenizeState (Array { tok :: Token , pos :: Pos })
tokenize' = do
  t <- readToken
  case t of
    M.Nothing -> pure []
    M.Just t' -> tokenize' >>= (\rest -> pure $ cons t' rest)

readToken :: TokenizeState (M.Maybe { tok :: Token , pos :: Pos })
readToken = do
  s <- S.get
  let startPos = s.pos
  c <- readChar
  case c of
    M.Nothing -> pure M.Nothing
    M.Just c' -> case singletonToken c' of
      M.Just t -> pure $ M.Just { tok : t, pos : startPos }
      M.Nothing -> do
        S.modify_ (\st -> st { buf = snoc s.buf c' })
        tok <- readUntilEnd
        pure $ decideToken tok startPos

decideToken :: String -> Pos -> (M.Maybe { tok :: Token, pos :: Pos })
decideToken str pos = 
  case N.fromString str of
    M.Just n -> M.Just { tok : Num n, pos : pos }
    M.Nothing -> if isQuotedStr str then
      M.Just { tok : Str str, pos : pos }
      else M.Just { tok : atomicToken str, pos : pos }

atomicToken :: String -> Token
atomicToken "true" = True
atomicToken "false" = False
atomicToken "null" = Null
atomicToken s = FieldName s

isQuotedStr :: String -> Boolean
isQuotedStr s =
  case R.regex "^(\".*?\")|(\'.*?\')$" noFlags of
    E.Left err -> trace err (\_ -> false)
    E.Right r -> R.test r s

readUntilEnd :: TokenizeState String
readUntilEnd = do
  ended <- tokenEnds
  s <- S.get
  if ended then do
    let tok = fromCodePointArray s.buf
    S.modify_ (\st -> st { buf = [] })
    pure tok
  else
    readUntilEnd

tokenEnds :: TokenizeState Boolean
tokenEnds = do
  s <- S.get
  peek <- peekChar
  case peek of
    M.Nothing -> pure true
    M.Just head -> do
      case s.quote of
        M.Just q -> do
          if head == q then do
            _ <- readChar
            S.modify_ (\st -> st { buf = snoc s.buf head, quote = M.Nothing })
            pure true
          else do
            _ <- readChar
            S.modify_ (\st -> st { buf = snoc s.buf head })
            pure false
        M.Nothing -> do
          if M.isJust (singletonToken head) then do
            pure true
          else do 
            _ <- readChar
            S.modify_ (\st -> st { buf = snoc s.buf head })
            pure false

singletonToken :: CodePoint -> M.Maybe Token
singletonToken c = case singleton c of
  "," -> M.Just Comma
  ":" -> M.Just Colon
  "{" -> M.Just ObjStart
  "}" -> M.Just ObjEnd
  "[" -> M.Just ArrStart
  "]" -> M.Just ArrEnd
  " " -> M.Just Space
  "\n" -> M.Just Space
  _ -> M.Nothing

readChar :: TokenizeState (M.Maybe CodePoint)
readChar = do
  s <- S.get
  case  uncons s.rest of
    M.Nothing -> pure M.Nothing
    M.Just { head : h, tail : t } -> do
      let { row : r, col : c } = s.pos
      if h == codePointFromChar '\n' then
        S.modify_ (\st -> st { pos { row = r + 1, col = 0 }})
      else
        S.modify_ (\st -> st { pos { col = c + 1 }})
      when (h == codePointFromChar '\"' || h == codePointFromChar '\'') do
        case s.quote of
          M.Nothing -> do S.modify_ (\st -> st { quote = M.Just h })
          M.Just q -> when (q == h) do S.modify_ (\st -> st { quote = M.Nothing })
      S.modify_ (\st -> st { rest = t })
      pure $ M.Just h

peekChar :: TokenizeState (M.Maybe CodePoint)
peekChar = do
  s <- S.get
  pure $ A.head s.rest

data Json = JStr String
          | JNum Number
          | JObj (Array { fieldName :: String, val :: JsonWPos })
          | JArr (Array JsonWPos)
          | JNull
          | JBool Boolean

instance showJson :: Show Json where
  show (JStr str) = str
  show (JNum num) = show num
  show (JObj items) = "{" <> (foldr (\a b -> a <> ", " <> b) "" (map show items)) <> "}"
  show (JArr items) = "[" <> (foldr (\a b -> a <> ", " <> b) "" (map show items)) <> "]"
  show JNull = "null"
  show (JBool true) = "true"
  show (JBool false) = "false"

type JsonWPos = { json :: Json, pos :: Pos }
type TokArr = Array { tok :: Token , pos :: Pos } 
type TRes = T.Tuple JsonWPos TokArr
type TokParser = TokArr -> M.Maybe TRes

parseJson :: Lazy TokParser
parseJson = defer \_ -> parseEither parseObject $ parseEither parseArr parseVal

parseVal :: TokParser
parseVal toks = case uncons toks of
  M.Just { head : h, tail : t } -> (parseVal' h.tok t) >>= (\(T.Tuple j r) -> M.Just $ T.Tuple { json : j, pos : h.pos } r)
  M.Nothing -> M.Nothing
  where
    parseVal' (Str s) rest = M.Just $ T.Tuple (JStr s) rest
    parseVal' (Num n) rest = M.Just $ T.Tuple (JNum n) rest
    parseVal' True rest = M.Just $ T.Tuple (JBool true) rest
    parseVal' False rest = M.Just $ T.Tuple (JBool false) rest
    parseVal' Null rest = M.Just $ T.Tuple JNull rest
    parseVal' _ _ = M.Nothing

parseArr :: TokParser
parseArr toks = do
  T.Tuple start rest <- parseOne isArrStart toks
  T.Tuple _ rest <- parseAny isSpace rest
  T.Tuple es rest <- parseArrElms rest
  T.Tuple _ rest <- parseOne isArrEnd rest
  pure $ T.Tuple { json : JArr es , pos : start.pos } rest

parseArrElms :: TokArr -> M.Maybe (T.Tuple (Array JsonWPos) TokArr)
parseArrElms toks =
  case parseArrElm toks of
      M.Just (T.Tuple e r) -> case (parseOne isComma r) >>= (\(T.Tuple _ rest) -> parseArrElms rest) of
                                                           M.Just (T.Tuple es rest) -> pure $ T.Tuple (cons e es) rest
                                                           M.Nothing -> pure $ T.Tuple [e] r
      M.Nothing -> pure $ T.Tuple [] toks

parseArrElm :: TokArr -> M.Maybe (T.Tuple JsonWPos TokArr)
parseArrElm toks = do
  T.Tuple _ rest <- parseAny isSpace toks
  T.Tuple e rest <- force parseJson rest
  T.Tuple _ rest <- parseAny isSpace rest
  pure $ T.Tuple e rest

parseObject :: TokParser
parseObject toks = do
  T.Tuple start rest <- parseOne isObjStart toks
  T.Tuple fs rest <- parseFields rest
  T.Tuple _ rest <- parseOne isObjEnd rest
  pure $ T.Tuple { json : JObj fs , pos : start.pos } rest

parseFields :: TokArr -> M.Maybe (T.Tuple (Array { fieldName :: String, val :: JsonWPos }) TokArr)
parseFields toks = 
  case parseField toks of
      M.Just (T.Tuple f r) -> case (parseOne isComma r) >>= (\ (T.Tuple _ rest) -> parseFields rest) of
                                                           M.Just (T.Tuple fs rest) -> pure $ T.Tuple (cons f fs) rest
                                                           M.Nothing -> pure $ T.Tuple [f] r
      M.Nothing -> pure $ T.Tuple [] toks

parseField :: TokArr -> M.Maybe (T.Tuple { fieldName :: String, val :: JsonWPos } TokArr)
parseField toks = do
   T.Tuple _ rest <- parseAny isSpace toks
   T.Tuple n rest <- parseFieldName rest
   T.Tuple _ rest <- parseAny isSpace rest
   T.Tuple _ rest <- parseOne isColon rest
   T.Tuple _ rest <- parseAny isSpace rest
   T.Tuple val rest <- force parseJson rest
   T.Tuple _ rest <- parseAny isSpace rest
   pure $ T.Tuple { fieldName : n, val : val } rest

parseFieldName :: TokArr -> M.Maybe (T.Tuple String TokArr)
parseFieldName toks = do
  case parseOne isFieldName toks of
    M.Just (T.Tuple { tok : (FieldName name), pos : _} rest) -> pure $ T.Tuple name rest
    _ -> case parseOne isStr toks of
      M.Just (T.Tuple { tok : (Str str), pos : _ } rest) -> pure $ T.Tuple str rest
      _ -> M.Nothing

-- Utils
isFieldName :: Token -> Boolean
isFieldName (FieldName _) = true
isFieldName _ = false

isStr :: Token -> Boolean
isStr (Str _) = true
isStr _ = false

isSpace :: Token -> Boolean
isSpace Space = true
isSpace _ = false

isColon :: Token -> Boolean
isColon Colon = true
isColon _ = false

isComma :: Token -> Boolean
isComma Comma = true
isComma _ = false

isObjStart :: Token -> Boolean
isObjStart ObjStart = true
isObjStart _ = false

isObjEnd :: Token -> Boolean
isObjEnd ObjEnd = true
isObjEnd _ = false

isArrStart :: Token -> Boolean
isArrStart ArrStart = true
isArrStart _ = false

isArrEnd :: Token -> Boolean
isArrEnd ArrEnd = true
isArrEnd _ = false

-- Parser Combinators 

parseOne :: (Token -> Boolean) -> TokArr -> M.Maybe (T.Tuple { tok :: Token, pos :: Pos } TokArr)
parseOne f toks = do
  { head : h , tail : t } <- uncons toks
  if f h.tok then
    pure $ T.Tuple h t
  else
    M.Nothing

parseAny :: (Token -> Boolean) -> TokArr -> M.Maybe (T.Tuple (Array { tok :: Token, pos :: Pos })  TokArr)
parseAny f toks = 
  case parseOne f toks of
      M.Just (T.Tuple t r) -> (parseAny f r) >>= (\ (T.Tuple ts rest) -> M.Just $ T.Tuple (cons t ts) rest )
      M.Nothing -> pure $ T.Tuple [] toks

parseEither :: TokParser -> TokParser -> TokParser
parseEither a b toks =
  case a toks of
    M.Just r -> M.Just r
    M.Nothing -> b toks

