{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Aeson.Extra.Stream
-- Copyright   :  (C) 2015-2016 Oleg Grenrus
-- License     :  BSD3
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
module Data.Aeson.Extra.Stream (
    streamDecode,
    ) where

import Prelude ()
import Prelude.Compat

import Control.Applicative (many, (<|>))
import Data.Aeson          (FromJSON, Result (..), Value, fromJSON)
import Data.Aeson.Parser   (value)

import qualified Data.Attoparsec.ByteString.Char8 as A8
import qualified Data.Attoparsec.ByteString.Lazy  as A
import qualified Data.ByteString.Lazy             as LBS

streamParse :: LBS.ByteString -> ([Value], Maybe String)
streamParse = start
  where
    start bs = case A.parse (lexemeChar '[') bs of
        A.Done bs' _    -> first bs'
        A.Fail _ _ err  -> ([], Just err)
    first bs = case A.parse (lexemeChar ']') bs of
        A.Done {}       -> ([], Nothing)
        A.Fail {}       -> go bs
    go bs = case A.parse valueEnd bs of
        A.Done _   (r, False) -> ([r], Nothing)
        A.Done bs' (r, True)  -> case go bs' of
            ~(rs, end)  -> (r:rs, end)
        A.Fail _ _ err  -> ([], Just err)
    valueEnd = do
        v <- value
        c <- True <$ lexemeChar ',' <|> False <$ lexemeChar ']'
        return (v, c)
    lexemeChar c = many A8.space *> A8.char c <* many A8.space

-- | Lazyly parse 'LBS.ByteString' with top-level JSON array.
--
-- /Note:/ inspecting result's second field will force the list!
--
-- @
-- let ~(values, err) = 'streamDecode' bs
-- traverse_ processValue values
-- maybe (pure ()) printError err
-- @
--
-- @since 0.3.2.0
streamDecode :: forall a. FromJSON a => LBS.ByteString -> ([a], Maybe String)
streamDecode bs = go values
  where
    (values, err)  = streamParse bs
    go :: [Value] -> ([a], Maybe String)
    go []     = ([], err)
    go (v:vs) = case fromJSON v of
        Error err'  -> ([], Just err')
        Success x  -> case go vs of
            ~(xs, err') -> (x:xs, err')
