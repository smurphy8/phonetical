{-# LANGUAGE OverloadedStrings #-}


module Data.Text.PhoneticalSpec (main, spec) where

import Data.Aeson
import Data.Aeson.Parser
import Data.Attoparsec hiding (Result) 
import Data.ByteString
import Data.Text
import Data.Text.Phonetical
import Data.Text.Phonetical.Internal
import Test.Hspec



main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseJSON Dictionary" $ do
    it "should parse JSON into a dictionary" $ do
      True `shouldBe` False


dictString :: ByteString
dictString  = "[{ \"find\":\".\",\"use\":\"dot\"},{\"find\":{\"start\":\"O(\",\"end\":\")\"}, \"use\":\"Big O of \"}]"

testParse :: (Result [DictionaryEntry])
testParse = case (parseOnly json dictString) of
  Left s -> fail s
  Right r -> fromJSON r

testEntry = [(Find (Simple (Symbol ".")) " dot ") , (Find (Wrapped (Symbol "O(") (Symbol ")")) "Big O of")]

testString :: Text
testString = "x.y results in O(n)"




