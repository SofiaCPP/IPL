module Lib
  ( golangToHtml
  ) where

import qualified Data.Text        as T

import           Data.Highlighter (tokensToHtml)
import           Data.Lexer       (tokenize)

golangToHtml :: T.Text -> T.Text
golangToHtml code = tokensToHtml $ tokenize code 0 0
