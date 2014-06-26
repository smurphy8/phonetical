{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Data.Text.Phonetical.Internal
    (
      Symbol (..) ,
      DictionaryExpr (..) ,
      DictionaryEntry (..),
      Dictionary
      
    ) where

import Control.Applicative
import Data.Aeson
import Data.Text

newtype Symbol = Symbol { getSymbol:: Text}
                      deriving (Eq,Show)



data DictionaryExpr =  Wrapped Symbol Symbol 
                      | Simple Symbol
                      deriving (Eq,Show)


data DictionaryEntry =  Find DictionaryExpr Text
                     deriving (Eq, Show)
                              
type Dictionary = [ DictionaryEntry ]

instance ToJSON DictionaryExpr where
  toJSON (Wrapped (Symbol s) (Symbol e ) ) = object ["start" .= s , "end" .= e]
  toJSON (Simple (Symbol s) ) = toJSON s

instance FromJSON DictionaryExpr where
  parseJSON (Object o) = do
    (o .: "start" >>= (\s ->
                        o .: "end" >>= (\e ->
                                         return $ Wrapped (Symbol s)  (Symbol e) ))) 
    <|> ( fail "Expecting Object to be start and end tokens")
  parseJSON (String t) = return . Simple . Symbol $ t
  parseJSON _ = fail "Rule: Expecting String or Object"    


instance ToJSON DictionaryEntry where
  toJSON (Find de t) = object [ "find" .= (toJSON de) , "use" .= (toJSON t)]

instance FromJSON DictionaryEntry where
  parseJSON (Object o) = do
    o .: "find" >>= (\f -> do
                      r <- parseJSON f
                      o .: "use" >>= (\u -> return $ Find r u))
  parseJSON _ = fail "Rule: Expecting Object received other"
  
    
