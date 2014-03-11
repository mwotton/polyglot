{-# LANGUAGE OverloadedStrings #-}
module Language.Polyglot where

import           Control.Arrow (second, (&&&))
import           Data.List     (group, sort, sortBy)
import qualified Data.Map      as Map
import           Data.Maybe    (fromMaybe)
import           Data.Ord
import           Data.Text     (Text)
import qualified Data.Text     as T
import           Debug.Trace

data Hole=Hole

type PolyModel = Text -> [(Text, Double)]
type Bigram = Text

build :: [(Text, Text)] -> PolyModel
build inputs = \query -> let
  res = sortBy (comparing (neg . snd)) $
        map (second (compute query)) normalised
  in trace (show res) res
  where bigrams = map (T.take 2) . T.tails
        neg x = x * (-1)
        occurrences = map (head &&& length) . group . sort . bigrams
        normalised :: [(Text, Bigram -> Double)]
        normalised = map (second makeEval) inputs
        makeEval text = let os = occurrences text
                            total :: Double
                            total = sum $ map (fromIntegral . snd) os
                            dict = Map.fromList $ map (second (\l -> fromIntegral l/total)) os
                        in (fromMaybe 0 . flip Map.lookup dict)
        average x = sum x / fromIntegral (length x)
        compute query evaluator = average (map evaluator $ bigrams query)


predict :: PolyModel -> Text -> Text
predict m = fst . Prelude.head  . m
