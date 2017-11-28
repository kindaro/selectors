-- | This module exports functions for parsing and executing CSS selector
-- expressions in pure Haskell. TH QuasiQuoters are provided in
-- "Text.XML.Selectors.CSS.TH" for validation and static-checking of CSS selectors.

{-# LANGUAGE OverloadedStrings #-}
module Text.XML.Selectors.CSS (
    toAxis,
    parsePath
    ) where

import Text.XML.Selectors.CSS.Parse
import Text.XML.Selectors.CSS.Types
import Text.XML
import Text.XML.Cursor
import Data.List
import Data.String
import qualified Data.Map as M
import qualified Data.Text as T

-- | Convert CSS 'Selector' to an 'Axis'.
toAxis :: Selector -> Axis
toAxis selector = descendant >=> toAxis' selector

mhead :: Monad m => [a] -> m a
mhead [] = fail "empty"
mhead (a:_) = return a

toAxis' (Selector selector) = simpleAxis selector
toAxis' (Combinator simple comb selector) = axis where
    axis = simpleAxis simple >=> combaxis >=> toAxis' selector
    combaxis = case comb of
        Descendant -> descendant
        Child -> child
        AnySibling -> followingSibling
        FollowingSibling -> mhead . followingSibling

simpleAxis (SimpleSelector mbelem specs mbpseudo) = axis where
    axis = elemaxis >=> specaxis >=> pseuaxis
    elemaxis = case mbelem of
        Nothing -> anyElement
        Just nm -> element (fromString nm)
    pseuaxis = case mbpseudo of
        Nothing -> return
        Just FirstChild -> mhead . child
        Just LastChild -> return . last . child
        Just (NthChild n) -> return . (!! (n-1)) . child
    specaxis = loop specs
    loop [] = return
    loop (spec:ss) = toaxis spec >=> loop ss
    toaxis (ID id) = attributeIs "id" (fromString id)
    toaxis (Class cls) = toaxis (Attrib "class" $ Pred Includes cls)
    toaxis (Attrib attr pred) = \c -> case node c of
        NodeElement (Element _ as _) | Just v <- M.lookup (fromString attr) as -> case pred of
            None -> [c]
            Pred op val | Equals <- op, val' == v -> [c]
                        | Includes <- op, val' `elem` T.words v -> [c]
                        | BeginsWith <- op, val' == T.take vallen v -> [c]
                        | EndsWith <- op, val' == T.drop (T.length v - vallen) v -> [c]
                        | otherwise -> [] where
                val' = fromString val
                vallen = T.length val'
        _ -> []
