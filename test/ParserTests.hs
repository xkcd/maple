{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ParserTests where

import qualified Data.Attoparsec.Text as P
import           Data.Text (Text)
import           Maple.TextHintParser
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.TestVector

checkHintLang :: Text -> [Text] -> Maybe () -> Bool
checkHintLang hint s r =
  either (error . ("did not parse hint format: " <>)) (\p -> p s == r) $ textHintLang () hint

-- | Just some simple 2d cases
testHintParser :: TestTree
testHintParser = testGroup "Hint Parser"
  [ testCase "Single value matcher" $ do
      let ap = fmap (\f -> f (\_ _ -> Just ()) mempty ["aba"]) $
               P.parseOnly (anyTextSymbol textSymbol) $ "@aba"
      ap @?= (Right (Just ()))
  , testVectors "Single value matches" (checkHintLang "@aba")
    [ (["aba"], Just ())
    , (["aba","bab"], Just ())
    , (["bab"],  Nothing)
    , ([], Nothing)
    , (["bab","aba"], Nothing)
    ]
  , testVectors "List value matches" (checkHintLang "@aba,@bab") $
    [ (["aba"], Nothing)
    , (["bab"], Nothing)
    , (["aba","bab"],  Just ())
    , (["aba","bab","cab"],  Just ())
    , (["cab","aba","bab"], Nothing)
    , (["cab","aba","bab","cab"], Nothing)
    , (["bab","aba"], Nothing)
    ]
  , testVectors "Set value matches" (checkHintLang "{aba|bab}") $
    [ (["aba"], Just ())
    , (["bab"], Just ())
    , (["aba","cab"], Just ())
    , (["bab","cab"], Just ())
    , (["cab"], Nothing)
    , (["cab","aba"], Nothing)
    , (["cab","bab"], Nothing)
    ]
  , testVectors "List set matches at start" (checkHintLang "{aba|bab},@bab") $
    [ (["aba"], Nothing)
    , (["bab"], Nothing)
    , (["aba","bab"], Just ())
    , (["bab","bab"], Just ())
    , (["aba","cab"], Nothing)
    , (["cab","bab"], Nothing)
    , (["cab","aba"], Nothing)
    , (["cab","aba","bab","cab"], Nothing)
    , (["aba","bab","cab"], Just ())
    , (["bab","bab","cab"], Just ())
    ]
  , testVectors "List set matches later" (checkHintLang "@cab,{aba|bab}") $
    [ (["aba"], Nothing)
    , (["bab"], Nothing)
    , (["cab"], Nothing)
    , (["cab","aba"], Just ())
    , (["cab","bab"], Just ())
    , (["aba","cab"], Nothing)
    , (["bab","cab"], Nothing)
    , (["cab","aba","bab"], Just ())
    , (["cab","bab","aba"], Just ())
    ]
  , testVectors "Variable matches anything" (checkHintLang "$A") $
    [ (["aba"], Just ())
    , (["bab"], Just ())
    , (["cab"], Just ())
    , ([], Nothing)
    , (["aba","bab","cab"], Just ())
    ]
  , testVectors "Variable checks equality" (checkHintLang "$A,$A") $
    [ (["aba","aba"], Just ())
    , (["aba","bab"], Nothing)
    , (["aba"], Nothing)
    , ([], Nothing)
    , (["aba","aba","bab"], Just ())
    ]
  , testVectors "Two variables" (checkHintLang "$A,$B") $
    [ (["aba"], Nothing)
    , (["aba","aba"], Nothing)
    , (["aba","bab"], Just ())
    , (["aba","aba","bab"], Nothing)
    , (["aba","bab","bab"], Just ())
    ]
  , testVectors "Variable of set matches" (checkHintLang "$A{aba|bab}") $
    [ (["aba"], Just ())
    , (["bab"], Just ())
    , (["cab"], Nothing)
    , ([], Nothing)
    , (["aba","bab","cab"], Just ())
    , (["bab","aba","cab"], Just ())
    , (["cab","aba"], Nothing)
    ]
  , testVectors "Variable of set checks equality" (checkHintLang "$A{aba|bab},$A") $
    [ (["aba","aba"], Just ())
    , (["aba","bab"], Nothing)
    , (["aba"], Nothing)
    , ([], Nothing)
    , (["aba","aba","bab"], Just ())
    ]
  , testVectors "Two variable sets" (checkHintLang "$A{aba|bab},$B{aba|bab}") $
    [ (["aba"], Nothing)
    , (["aba","aba"], Nothing)
    , (["aba","bab"], Just ())
    , (["aba","aba","bab"], Nothing)
    , (["aba","bab","bab"], Just ())
    ]
  , testVectors "Variable of set with open variable" (checkHintLang "$A{aba|bab},$B") $
    [ (["aba"], Nothing)
    , (["aba","aba"], Nothing)
    , (["aba","bab"], Just ())
    , (["aba","aba","bab"], Nothing)
    , (["aba","bab","bab"], Just ())
    , (["bab"], Nothing)
    , (["bab","bab"], Nothing)
    , (["bab","aba"], Just ())
    , (["bab","bab","aba"], Nothing)
    , (["bab","aba","aba"], Just ())
    ]
  , testVectors "Variable of set matches" (checkHintLang "$A{aba|bab}") $
    [ (["aba"], Just ())
    , (["bab"], Just ())
    , (["cab"], Nothing)
    , ([], Nothing)
    , (["aba","bab","cab"], Just ())
    , (["bab","aba","cab"], Just ())
    , (["cab","aba"], Nothing)
    ]
  , testVectors "List of set matches" (checkHintLang "{aba|bab},{aba|bab}") $
    [ (["aba"], Nothing)
    , (["bab"], Nothing)
    , (["cab","aba","bab"], Nothing)
    , (["aba","aba"], Just ())
    , (["aba","bab"], Just ())
    , (["bab","bab"], Just ())
    , (["bab","aba"], Just ())
    , (["aba","aba","bab"], Just ())
    , (["aba","bab","bab"], Just ())
    , (["aba","cab","aba"], Nothing)    
    ]
  , testGroup "elipses" $
    [ testVectors "after var, at end" (checkHintLang "@aba,...") $
      [ (["aba"], Just ())
      , (["aba","bab"], Just ())
      , (["aba","bab","cab"], Just ())
      , ([], Nothing)
      , (["bab"], Nothing)
      , (["bab","aba"], Nothing)
      ]
    , testVectors "before var, at start" (checkHintLang "...,@aba") $
      [ (["aba"], Just ())
      , (["aba","bab"], Just ())
      , (["aba","bab","cab"], Just ())
      , ([], Nothing)
      , (["bab"], Nothing)
      , (["bab","aba"], Just ())
      , (["bab","cab","aba"], Just ())
      , (["bab","aba","cab"], Just ())
      ]
    , testVectors "var between sets" (checkHintLang "{aba|cab},...,@bab,...,{aba|cab}") $
      [ (["aba"], Nothing)
      , (["bab"], Nothing)
      , (["bab","aba"], Nothing)
      , (["aba","bab"], Nothing)
      , (["aba","bab","cab"], Just ())
      , (["cab","bab","aba"], Just ())
      , ([], Nothing)
      , (["aba","d","bab","cab"], Just ())
      , (["aba","bab","d","cab"], Just ())
      , (["aba","d","bab","d","cab"], Just ())
      ]
    ]
  , testVectors "Number set value matches" (checkHintLang "{123|586}") $
    [ (["123"], Just ())
    , (["586"], Just ())
    , (["123","411"], Just ())
    , (["586","411"], Just ())
    , (["411"], Nothing)
    , (["411","123"], Nothing)
    , (["411","586"], Nothing)
    ]
  ]
