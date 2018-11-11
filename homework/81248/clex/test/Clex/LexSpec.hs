module Clex.LexSpec
    ( spec
    ) where

import qualified Data.ByteString       as BS
import           Data.ByteString       (ByteString)
import           Data.ByteString.Char8 (pack)

import           Clex.Types
import           Clex.Lex   (lexC)

import           Test.QuickCheck
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Clex.TypesSpec        ()

spec :: Spec
spec = describe "lexing" $ do
    prop "tokensToBS after lexC is id" $
        \x -> let x' = unValidC x in
            case lexC x' of
            (Left _) -> False
            (Right x'') -> x' == tokensToBS x''
    prop "lexing after tokensToBS is id" $
        \x -> let x' = unTokenC x in
            case lexC $ tokensToBS x' of
            (Left _) -> False
            (Right x'') -> x' == x''

newtype TokenC = TokenC {unTokenC :: [Token]}
    deriving (Eq, Show)

newtype ValidC = ValidC {unValidC :: ByteString}
    deriving (Eq, Show)

instance Arbitrary TokenC where
    arbitrary = TokenC . packWhitespaceTokens . concat <$>
        listOf1 (do
            x <- arbitrary
            whitespace <- whitespaceGen
            pure [x, whitespace])
        where
            whitespaceGen = Whitespace . pack <$> do listOf1 $ elements ['\t', ' ', '\n']

instance Arbitrary ValidC where
    arbitrary = ValidC . BS.concat . map tokenToBS <$> listOf1 (frequency
        [ (30, arbitrary)
        , (60, whitespaceGen)
        ])
        where
            whitespaceGen = Whitespace . pack <$> do listOf1 $ elements ['\t', ' ', '\n']

packWhitespaceTokens :: [Token] -> [Token]
packWhitespaceTokens []  = []
packWhitespaceTokens [x] = [x]
packWhitespaceTokens (Whitespace x : Whitespace y : xs)
    = packWhitespaceTokens $ Whitespace (x <> y) : xs
packWhitespaceTokens (x:xs) = x : packWhitespaceTokens xs
