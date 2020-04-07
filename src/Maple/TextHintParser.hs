{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Maple.TextHintParser where

import           Control.Monad
import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as P
import           Data.Hashable
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import           System.Random

-- | text symbols: Matches a piece of text containing something in the set of [a-zA-Z0-9/_] (non-empty)
textSymbol :: Parser Text
textSymbol = P.takeWhile1 (P.inClass "a-zA-Z0-9/_") P.<?> "text symbol"

textHintLang :: r -> Text -> Either String ([Text] -> Maybe r)
textHintLang r = P.parseOnly (textHintParser textSymbol r)

parseTextTumbler :: forall r . [r] -> Text -> Either String ([Text] -> Maybe r)
parseTextTumbler rs t =
  fmap randomizer $ textHintLang () t
  where
    randomizer :: ([Text] -> Maybe ()) -> [Text] -> Maybe r
    randomizer decider s =
      fmap (const $ rs!!(fst $ randomR (0, length rs-1) (mkStdGen (hash s)))) $ decider s

-- | Pattern matching
--   @_    : matches some symbol _
--   $A_   : matches any complete symbol that no other variable in scope currently matches as the variable A.
--           Can be constrained to be in the, optional, _ pattern (Either @ or {})
--           Legal variable names are in [a-zA-z0-9_].
--   {a,b} : matches a symbol in the set of a and b.
--   [a,b] : matche she pattern a and matches the pattern b.
--   ...   : Skips 0 or more list entries.
--
--   lexically scoped variables.
textHintParser :: Eq s => Parser s -> r -> Parser ([s] -> Maybe r)
textHintParser ps r = (\f -> f (\_ _ -> Just r) mempty) <$> (parseListBody ps <* P.endOfInput)

data VarValue s
 = VarSym s
 deriving (Eq, Ord, Read, Show)

type ParserCont s r = (Map Text (VarValue s) -> [s] -> Maybe r) -> Map Text (VarValue s) -> [s] -> Maybe r

parseListBody :: Eq s => Parser s -> Parser (ParserCont s r)
parseListBody ps = do
  conts <- (parseListEntry ps) `P.sepBy1'` (P.char ',')
  pure $ \cont vmap slist -> (foldr (\upper lower -> upper lower) cont conts) vmap slist

parseListEntry :: Eq s => Parser s -> Parser (ParserCont s r)
parseListEntry ps = do
  P.choice
    [ anyTextSymbol ps
    , inSymbolSet ps
    , varMatcher ps
    , elipseMatcher
    ] P.<?> "List entry"

-- | A type that represents setting the value of a variable and returning the unused portion of the list
--   if we managed to match.
type VarAssigner s = [s] -> Maybe (VarValue s, [s])

-- | Given a Var, checks if it matches and if it does, returns Just the unused tail.
checkVar :: Eq s => VarValue s -> [s] -> Maybe [s]
checkVar (VarSym _) [] = Nothing
checkVar (VarSym v) (h:t) = if v == h then Just t else Nothing

parseVarAssigner :: Eq s => Parser s -> Parser (VarAssigner s)
parseVarAssigner ps = P.choice
  [ parseVarSet
  , parseNoVarRestriction
  ] P.<?> "Var assigner"
  where
    parseVarSet = do
      sset <- (P.char '{') *> ps `P.sepBy1'` (P.char '|') <* (P.char '}')
      pure $ \case
        (h:t) | h `elem` sset -> Just (VarSym h, t)
        _ -> Nothing
    parseNoVarRestriction = do
      me <- P.peekChar
      case me of
        -- End of string, so no specifier
        Nothing -> pure $ \case
          [] -> Nothing
          (h:t) -> Just (VarSym h, t)
        -- No specifier
        Just ',' -> pure $ \case
          [] -> Nothing
          (h:t) -> Just (VarSym h, t)
        Just _ -> mzero P.<?> "Not a valid variable assigner"

varMatcher :: Eq s => Parser s -> Parser (ParserCont s r)
varMatcher ps = do
  v <- P.char '$' *> P.takeWhile1 (P.inClass "a-zA-Z0-9_")
  vassigner <- parseVarAssigner ps
  pure $ \cont vmap slist ->
    case Map.lookup v vmap of
      -- New variable case
      Nothing ->
        case vassigner slist of
          Nothing -> mzero
          Just (assignment, t) ->
            if assignment `elem` (Map.elems vmap)
            then mzero
            else cont (Map.insert v assignment vmap) t
      -- We know it doesn't match another variable because we already assigned it
      Just assigned ->
        case checkVar assigned slist of
          Nothing -> mzero
          Just pf -> cont vmap pf

anyTextSymbol :: Eq s => Parser s -> Parser (ParserCont s r)
anyTextSymbol ps = do
  s <- P.char '@' *> ps
  pure $ \cont vmap slist ->
    case slist of
      (h:t) | h == s -> cont vmap t
      _ -> mzero

inSymbolSet :: Eq s => Parser s -> Parser (ParserCont s r)
inSymbolSet ps = do
  sset <- (P.char '{') *> ps `P.sepBy1'` (P.char '|') <* (P.char '}')
  pure $ \cont vmap slist ->
    case slist of
      (h:t) | h `elem` sset -> cont vmap t
      _ -> mzero

elipseMatcher :: Parser (ParserCont s r)
elipseMatcher = do
  void $ P.string $ T.pack "..."
  pure $ \cont vmap slist ->
    listToMaybe $
    mapMaybe (cont vmap) $
    tails slist
