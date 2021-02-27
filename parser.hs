module Parser where

import Data.Char
import Debug.Trace
import Control.Applicative

newtype Parser i o = Parser { runParser :: i -> (Maybe o, i) }

instance Functor (Parser i) where
  -- fmap :: (o -> o') -> Parser i o -> Parser i o'
  fmap f p = Parser $ \input ->
    let (mo, rest) = runParser p input
    in (fmap f mo, rest)

instance Applicative (Parser i) where
  pure x = Parser $ \input -> (Just x, input)

  -- (<*>) :: Parser i (o -> o') -> Parser i o -> Parser i o'
  pf <*> po = Parser $ \input -> case runParser pf input of
    (Nothing, _)   -> (Nothing, input)
    (Just f, rest) -> case runParser po rest of
      (Nothing, _)    -> (Nothing, input)
      (Just o, rest') -> (Just (f o), rest')


instance Alternative (Parser i) where
  empty        = Parser $ \i -> (Nothing, i)
  (<|>) p1 p2  = Parser $ \i -> case runParser p1 i of
                                      (Nothing, _) -> case runParser p2 i of
                                                          (Nothing, _) -> (Nothing, i)
                                                          x -> x -- (_, _ -> runParser p1, i)
                                      f -> f


type Digit = Int

digit :: Parser String Digit
digit = Parser $ \input -> traceShow input $ traceShowId $ case input of
  (c:cs) | isDigit c -> (Just $ digitToInt c, cs)
  cs                 -> (Nothing, cs)

wholeNumber :: Parser String Int
wholeNumber = pure (\num d -> d * 10^countDigits num + num) -- refer abhinav's code
              <*> digit
              <*> wholeNumber
  where
    countDigits = length . show


--wholeNumber :: Parser String Int
--wholeNumber = pure (\d num -> d * 10^countDigits num + num)
--              <*> digit
--              <*> wholeNumber
--  where
--    countDigits = length . show

-- wholeNumber = Parser $ \input -> case input of
--   "" -> (Nothing, "")
--   _ -> case runParser digit input of
--               (Just d, rest) -> case runParser wholeNumber rest of
--                                   (Just num, rest') -> (Just (d * 10^countDigits num + num), rest')
--                                   (Nothing, rest') -> (Just d, rest')
--               (Nothing, rest) -> (Nothing, rest)
--

char :: Char -> Parser String Char
char x = Parser $ \input -> case input of
  (c:cs) | c == x -> (Just c, cs)
  _               -> (Nothing, input)

word :: String -> Parser String String
word "" = Parser $ \input -> (Just "", input)
word (x:xs) = Parser $ \input -> case runParser (char x) input of
  (Just c, rest) -> case runParser (word xs) rest of
                      (Just w, rest') -> (Just (c:w), rest')
                      (Nothing, _) -> (Nothing, input)
  (Nothing, rest) -> (Nothing, rest)

space = isSpace

satisfy

data Value = JString String
            | JNumber Double
            | JObject [(String, Value)]
            | JArray [Value]
            | JBool Bool
            | JNull

json = jnull
      <|> jbool

--jarray = char '[' <*> many space *> json <* many space <*> char ','

jnull :: Parser String Value
jnull = word "null" *> pure JNull -- *> ignore result of LHS; return result of RHS

jbool :: Parser String Value
jbool = (word "true" *> pure (JBool True))
        <|> (word "false" *> pure (JBool False))
