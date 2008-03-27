-- Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
-- This code is made available under the terms of the Eclipse Public 
-- License, version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
-- 
module GHCOutputParser( parseBuffer ) where

import Control.Monad( liftM )
import Data.Maybe( fromMaybe )
import Text.ParserCombinators.Parsec

import Cohatoe.Data.MarkerDesc

parseBuffer :: String -> [MarkerDesc]
parseBuffer = run messages 


-- helping functions
--------------------

run :: Parser [MarkerDesc] -> String -> [MarkerDesc]
run p input = case (parse p "" input) of
  Left err -> error $ show err
  Right xs -> xs

mkMarkerDesc :: String -> Int -> Maybe Int -> Maybe Int -> String -> Severity -> MarkerDesc
mkMarkerDesc fn ln colFrom colTo msg sev = 
  MarkerDesc fn ln msg cf ct sev where
    cf = fromMaybe unspecified colFrom
    ct = fromMaybe unspecified colTo

messages :: Parser [MarkerDesc]
messages = do
  ignoreLine "Chasing modules from: " 
  ignoreStuff
  msgs <- many $ try ghcMessage
  ignoreLine "Linking " 
  return msgs

ghcMessage :: Parser MarkerDesc
ghcMessage = do
  skipMany $ oneOf wsChars
  fn <- manyTill (noneOf ":") (char ':')
  ln <- liftM read (manyTill digit (char ':'))
  (colFrom, colTo) <- try colRange <|> try singleCol
  sev <- option Error warning
  msg <- (char '\n' >> tillEol) <|> (string "\r\n" >> tillEol) <|> tillEol
  -- bit of a hack: everything else with indent 4 seems to be additional info
  -- which we can't really render in markers
  ignoreLine "    "
  skipMany $ oneOf wsChars
  return $ mkMarkerDesc fn ln colFrom colTo msg sev

wsChars :: [Char]
wsChars = " \t\n\r"

warning :: Parser Severity
warning = do
  try $ many (oneOf wsChars) >> string "Warning:" 
  return Warning

colRange :: Parser (Maybe Int, Maybe Int)
colRange = do
  frm <- liftM read (many digit)
  char '-'
  t   <- liftM read (many digit)
  char ':'
  return (Just frm, Just t)

singleCol :: Parser (Maybe Int, Maybe Int)
singleCol = do
  frm <- liftM read (many digit)
  char ':'
  return (Just frm, Nothing)

ignoreLine :: String -> Parser ()
ignoreLine start = skipMany $ try ( string start >> tillEol )

ignoreStuff :: Parser ()
ignoreStuff = skipMany $ try ( string "Compiling " >> tillEol ) 
    <|> ( counter >> string " Compiling " >> tillEol )
    <|> ( string "Skipping " >> tillEol )

counter :: Parser ()
counter = do
  char '['
  many digit
  string " of "
  many digit
  char ']'
  return ()

tillEol :: Parser String 
tillEol = manyTill (noneOf "\n") eol where
  eol = newline <|> (eof >> return '\n')
