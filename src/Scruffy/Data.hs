{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Scruffy.Data
    ( Album(..)
    , Band(..)
    , SearchResult(..)
    , bio
    , name
    , url
    , imageUrl
    , relatedBands
    , albums
    , year
    , rating
    , sanitizeBand
    , band
    ) where

import           Control.Lens

import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.Text     as T

import           GHC.Generics

data Band =
    Band { _url          :: T.Text
         , _name         :: T.Text
         , _bio          :: Maybe T.Text
         , _imageUrl     :: Maybe T.Text
         , _relatedBands :: [Band]
         , _albums       :: [Album]
         }
    deriving ( Show, Generic )

data Album =
    Album { _name     :: T.Text
          , _year     :: Maybe Int
          , _rating   :: Double
          , _band     :: Maybe Band
          , _imageUrl :: Maybe T.Text
          }
    deriving ( Show, Generic )

data SearchResult a =
    SearchResult { _data :: [a], _count :: Int }
    deriving ( Show, Generic )

deriveToJSON defaultOptions { fieldLabelModifier = drop 1 } ''Band

deriveToJSON defaultOptions { fieldLabelModifier = drop 1 } ''Album

deriveToJSON defaultOptions { fieldLabelModifier = drop 1 } ''SearchResult

makeFieldsNoPrefix ''Band

makeFieldsNoPrefix ''Album

sanitizeBand :: Band -> Band
sanitizeBand b = b & name %~ T.strip & bio . _Just %~ T.strip
