{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module UArith.Parse where

------------------------------------------------------------------------------
import Control.Monad
import Data.Text (Text)
import Text.Parsec
import UArith.Types
------------------------------------------------------------------------------

parse :: Text -> Either ParseError Term
parse t = runParser parseTerm () "input" t

parseTerm :: Monad m => ParsecT Text u m Term
parseTerm =
    parseZero
    <|> parseTrue
    <|> parseFalse
    <|> parseSucc
    <|> parsePred
    <|> try parseCond
    <|> try parseIsZero

ws = space >> spaces >> return ()

parseZero = char 'Z' >> return CZero
parseTrue = char 'T' >> return CTrue
parseFalse = char 'F' >> return CFalse
parseSucc = do
    char 'S'
    ws
    t <- parseTerm
    return $ ESucc t
parsePred = do
    char 'P'
    ws
    t <- parseTerm
    return $ EPred t
parseCond = do
    string "if"
    ws
    c <- parseTerm
    ws
    string "then"
    ws
    a <- parseTerm
    ws
    string "else"
    ws
    b <- parseTerm
    return $ ECond c a b
parseIsZero = do
    string "isZero"
    ws
    t <- parseTerm
    return $ EIsZero t
