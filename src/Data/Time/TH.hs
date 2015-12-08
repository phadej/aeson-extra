{-# LANGUAGE TemplateHaskell #-}
-- | Template Haskell extras for `Data.Time`.
--
-- This module is in a wrong place, but it reuses the same parser as aeson parser.
module Data.Time.TH (mkUTCTime) where

import Control.Applicative ((<*))
import Data.Time (UTCTime(..), Day(..))
import qualified Data.Text as T
import qualified Data.Attoparsec.Text as A
import Language.Haskell.TH
import Data.Aeson.Extra.Time (utcTime)

-- | Make  a 'UTCTime'. Accepts the same strings as aeson `UTCTime` parser accepts.
--
-- > t :: UTCTime
-- > t = $(mkUTCTime "2014-05-12 00:02:03.456000Z")
mkUTCTime :: String -> Q Exp
mkUTCTime s =
    case A.parseOnly (utcTime <* A.endOfInput) (T.pack s) of
        Left err -> fail $ "could not parse UTCTime: " ++ err
        Right (UTCTime (ModifiedJulianDay d) dt) ->
            [| UTCTime (ModifiedJulianDay $(d')) $(dt') :: UTCTime |]
          where d'  = return $ LitE $ integerL d
                dt' = return $ LitE $ rationalL $ toRational dt
