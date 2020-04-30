{-# LANGUAGE OverloadedStrings #-}
module TimeStamp where

import qualified Data.Aeson as JS
import qualified Data.Text as T

import ADL.Core

import Data.Time.Clock(UTCTime)
import Data.Time.Format

newtype TimeStamp = TimeStamp {unTimeStamp :: UTCTime}
  deriving(Eq,Ord,Show)

instance AdlValue TimeStamp where
  atype _ = "TimeStamp"
  jsonGen = JsonGen
    ( JS.String
    . T.pack
    . formatTime defaultTimeLocale isoFormat
    . unTimeStamp
    )

  jsonParser = JsonParser parse
    where
      parse ctx (JS.String s) = case parseTimeM True defaultTimeLocale isoFormat (T.unpack s) of
        Right utcTime -> ParseSuccess (TimeStamp utcTime)
        _ -> ParseFailure "expected an timestamp in iso8601 string format" ctx
      parse ctx _ = ParseFailure "expected an timestamp in iso8601 string format" ctx

isoFormat = iso8601DateFormat (Just "%H:%M:%S%Q")
