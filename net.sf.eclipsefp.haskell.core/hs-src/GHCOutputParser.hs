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

data Loc = Loc (Maybe Int) (Maybe Int) (Maybe Int) (Maybe Int)

run :: Parser [MarkerDesc] -> String -> [MarkerDesc]
run p input = case (parse p "" input) of
  Left err -> error $ show err
  Right xs -> xs

mkMarkerDesc :: String -> Loc -> String -> Severity -> MarkerDesc
mkMarkerDesc fn (Loc lStart lEnd cStart cEnd) msg sev = 
  MarkerDesc fn (fm lStart) (fm lEnd) msg (fm cStart) (fm cEnd) sev where
    fm = fromMaybe unspecified

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
  loc <- location
  sev <- option Error warning
  msg <- (char '\n' >> tillEol) <|> (string "\r\n" >> tillEol) <|> tillEol
  -- bit of a hack: everything else with indent 4 seems to be additional info
  -- which we can't really render in markers
  ignoreLine "    "
  skipMany $ oneOf wsChars
  return $ mkMarkerDesc fn loc msg sev

wsChars :: [Char]
wsChars = " \t\n\r"

warning :: Parser Severity
warning = do
  try $ many (oneOf wsChars) >> string "Warning:" 
  return Warning

location :: Parser Loc
location = try parenthesizedLoc <|> sepLocs

sepLocs :: Parser Loc
sepLocs = do
  (lnStart, lnEnd) <- singleLine
  (colStart, colEnd) <- try colRange <|> try singleCol
  return $ Loc lnStart lnEnd colStart colEnd

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

parenthesizedLoc :: Parser Loc
parenthesizedLoc = do
  char '('
  lStart <- liftM read (many digit)
  char ','
  cStart <- liftM read (many digit)
  char ')'
  char '-'
  char '('
  lEnd <- liftM read (many digit)
  char ','
  cEnd <- liftM read (many digit)
  char ')'
  char ':'
  return $ Loc (Just lStart) (Just lEnd) (Just cStart) (Just cEnd)

singleLine :: Parser (Maybe Int, Maybe Int)
singleLine = do
  line <- liftM read (manyTill digit (char ':'))
  return (Just line, Nothing)

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
