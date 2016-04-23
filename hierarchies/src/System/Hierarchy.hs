{-#LANGUAGE ViewPatterns, ScopedTypeVariables #-}
module System.Hierarchy
    ( Hierarchy
    , Format(..)
    , render, parse--, normalize
    ) where

import Data.Default.Class
import Data.List
import Data.List.Split
import Data.Monoid
import Control.Applicative


data Hierarchy f
    = Rooted [Segment]
    | Relative [Segment]
data Segment
    = Decoded String
    -- | Encoded Text


-- minimal implementation: `separator` and one of `encode, encodeT`
class Format f where
    root :: f -> String
    root = separator
    separator :: f -> String
    -- perform any necessary escaping on a single piece of the path
    encode :: f -> String -> Maybe String -- FIXME failure may spit out a message
    --encode f = T.unpack . encodeT f . T.pack
    --encodeT :: f -> Text -> Text
    --encodeT f = T.pack . encode f . T.unpack
    -- undo any escaping for a single piece of the path
    decode :: f -> String -> Maybe String -- FIXME failure may spit out a message
    --decode f = (T.unpack <$>) . decodeT f . T.pack
    --decodeT :: f -> Text -> Maybe Text
    --decodeT f = (T.pack <$>) . decode f . T.unpack
    -- TODO bytestring


instance Monoid (Hierarchy f) where
    mempty = Relative []
    (Relative as) `mappend` (Relative bs) = Relative (as <> bs)
    (Rooted as) `mappend` (Relative bs) = Rooted (as <> bs)
    _ `mappend` (Rooted bs) = Rooted bs
instance Default (Hierarchy f) where
    def = mempty
unit :: Format f => String -> Hierarchy f
unit x = Relative [Decoded x]
join :: Format f => Hierarchy f -> [String] -> Hierarchy f
join h xs = h <> Relative (Decoded <$> xs)

render :: forall f. Format f => Hierarchy f -> Maybe String
render h = do
    let (prefix, xs) = case h of
            Rooted xs -> (root f, xs)
            Relative xs -> ("", xs)
    body <- intercalate (separator f) <$> mapM renderSegment xs
    pure $ prefix <> body
    where
    renderSegment (Decoded x) = encode f x
    f = undefined :: f

parse :: forall f. Format f => String -> Maybe (Hierarchy f)
parse input = do
    let (str, ctor) = case input of
            (stripPrefix (root f) -> Just str) -> (str, Rooted)
            str -> (str, Relative)
        raw_segments = splitOn (separator f) str
    segments <- mapM (decode f) raw_segments
    pure $ ctor (Decoded <$> segments)
    where f = undefined :: f

-- normalize -- TODO would require up/here path segments




data UnixFilePath
instance Format UnixFilePath where
    separator _ = "/"
    encode _ = Just -- FIXME error on backslash or null byte
    decode _ = Just -- FIXME error on backslash or null byte
