{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Tasks.GADT_1.GADTParser where

import           Data.Text              (pack, unpack)
import           Tasks.GADT_1.GADTExpr
import           Text.Parsec.Char       (char, digit, satisfy, space, string)
import           Text.Parsec.Combinator (between, many1)
import           Text.Parsec.Language   (haskellDef)
import           Text.Parsec.Prim       (many, parseTest, try, (<|>))
import           Text.Parsec.Text       (Parser)
import           Text.Parsec.Token

iLitP :: Parser (Lit Int)
iLitP = (ILit . read) <$> (\x -> bracketP x <|> x) (spacedP (many1 digit))

bLitP :: Parser (Lit Bool)
bLitP = (\x -> BLit (if x == 'T' then True else False)) <$> (\x -> bracketP x <|> x) (spacedP (satisfy (\x -> x == 'T' || x == 'F')))

iiLitP :: Parser (Expr Int)
iiLitP = Lit <$> iLitP

bbLitP :: Parser (Expr Bool)
bbLitP = Lit <$> bLitP

addP :: Parser (Expr Int)
addP = (Add <$> (spacedP (bracketP parse) <* char '+') <*> spacedP parse) <|>
       (Add <$> (iiLitP <* char '+') <*> spacedP parse)

leqP :: Parser (Expr Bool)
leqP = Leq <$> (parse <* char '<') <*> parse

andP :: Parser (Expr Bool)
andP = (And <$> (spacedP (bracketP parse) <* string "&&") <*> spacedP parse) <|>
       (And <$> (bbLitP <* string "&&") <*> spacedP parse)

spacedP :: Parser a -> Parser a
spacedP p = (many space *> p) <* many space

bracketP :: Parser a -> Parser a
bracketP = try . between (char '(') (char ')')

class MyParse a where
  parse :: Parser (Expr a)

instance MyParse Int where
  parse = try (spacedP addP) <|> bracketP parse <|> iiLitP
instance MyParse Bool where
  parse = try (spacedP leqP) <|> try (spacedP andP) <|> bracketP parse <|> bbLitP
