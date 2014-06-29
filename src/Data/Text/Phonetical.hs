{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Data.Text.Phonetical where

import Data.Text.Phonetical.Internal
import Data.Attoparsec.Text
import qualified Data.List as L
import Data.Text
import Control.Applicative

phonetic :: Dictionary -> Text -> Text
phonetic dict istr = case (parseOnly (parseAllInDictionary dict) istr) of
  Left s -> pack s
  Right t -> t 


parseAllInDictionary :: Dictionary -> Parser Text
parseAllInDictionary dict = do
  plist <- many (mkReplacementParser dict)
  return $ Data.Text.concat plist
  

mkReplacementParser :: Dictionary -> Parser Text
mkReplacementParser dict = do
  (e,nmatchCharLst) <- anyTillAlt
  return (append (pack nmatchCharLst) e)

  return (append (pack nmatchCharLst) e)
  where
    anyTillAlt = manyTillWith anyChar altParser 
    altParser  = choice (mkParserList  dict)


manyTillWith :: Parser a -> Parser b -> Parser (b,[a])
manyTillWith p end = scn
  where
    scn = do
      ep <- eitherP end p
      case ep  of
        Left e -> return (e,[])
        Right c -> do
          (e,l) <- scn
          return (e,c:l)
      

        
mkParserList :: Dictionary -> [Parser Text]
mkParserList dict = L.foldl' parseBuild [] dict
  where
    parseBuild l (Find (Simple sym) txt ) = (replaceSimple sym txt):l
    parseBuild l (Find (Wrapped (Symbol iSym) (Symbol fSym)) txt ) = (replaceWrapped iSym fSym txt):l
    replaceSimple sym txt = (string . getSymbol $ sym) *> (pure txt)

    replaceWrapped isym fsym t = do
      _ <- string isym
      (e,cList) <- manyTillWith anyChar (string fsym)
      return  (append t (pack cList))


