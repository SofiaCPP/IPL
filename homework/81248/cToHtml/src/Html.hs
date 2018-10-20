{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Html
    ( cToHtml
    ) where

import qualified Data.Text as T

import Lucid

import Clex.Types

cToHtml :: String -> [Token] -> Html ()
cToHtml col toks = pre_ $ foldMap (colourize col) toks

colourize :: String -> Token -> Html ()
colourize _ (Whitespace bs) = toHtmlRaw bs
colourize _ (HexNumber  bs) = toHtml bs
colourize _ (OctNumber  bs) = toHtml bs
colourize _ (Number     bs) = toHtml bs
colourize _ (CharLit    bs) = toHtml bs
colourize _ (Scientific bs) = toHtml bs
colourize _ (DotFloat   bs) = toHtml bs
colourize _ (LeadingDot bs) = toHtml bs
colourize _ (StringLit  bs) = toHtml bs
colourize _ (Identifier bs) = toHtml bs
colourize col x = span_ [style_ $ "color:" <> col'] $ toHtml $ tokenToBS x
    where col' = T.pack col
