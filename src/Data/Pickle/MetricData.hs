{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Pickle.MetricData (MetricData(..), ShowMetricData(..)) where

import Data.Word (Word8, Word16, Word32, Word64)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Scientific (Scientific)
import Data.Time.Clock (DiffTime, NominalDiffTime)
import Data.Text (Text, pack)
import Numeric.Natural (Natural)

-- | Something that can be sent as a metric.
class MetricData a where
    renderMetricData :: a -> Text

newtype ShowMetricData a = ShowMetricData a
instance Show a => MetricData (ShowMetricData a) where
    renderMetricData (ShowMetricData a) = showT a

deriving via (ShowMetricData Int) instance MetricData Int
deriving via (ShowMetricData Int8) instance MetricData Int8
deriving via (ShowMetricData Int16) instance MetricData Int16
deriving via (ShowMetricData Int32) instance MetricData Int32
deriving via (ShowMetricData Int64) instance MetricData Int64
deriving via (ShowMetricData Double) instance MetricData Double
deriving via (ShowMetricData Integer) instance MetricData Integer
deriving via (ShowMetricData Natural) instance MetricData Natural
deriving via (ShowMetricData Word) instance MetricData Word
deriving via (ShowMetricData Word8) instance MetricData Word8
deriving via (ShowMetricData Word16) instance MetricData Word16
deriving via (ShowMetricData Word32) instance MetricData Word32
deriving via (ShowMetricData Word64) instance MetricData Word64
instance MetricData DiffTime where -- can't use show, as it appends "s" suffix, we also need miliseconds
    renderMetricData = showT . (*1000) . fromRational @Scientific . toRational
instance MetricData NominalDiffTime where -- can't use show, as it appends "s" suffix, we also need miliseconds
    renderMetricData = showT . (*1000) . fromRational @Scientific . toRational

-- | Internal utility to show something as Text
showT :: (Show a) => a -> Text
showT = pack . show
