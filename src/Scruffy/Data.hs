{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Scruffy.Data
    ( Album(..)
    , Band(..)
    , SearchResult(..)
    , Error(..)
    , convertToServantErr
    , liftMaybeToError
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
import           Control.Monad
import           Control.Monad.Except

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Maybe
import qualified Data.Text                   as T

import           GHC.Generics

import           Servant.Server

import           Text.Blaze.Html
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as HA
import           Text.Blaze.Internal

data Error = NotFound

liftMaybeToError :: MonadError Error m => Maybe a -> m a
liftMaybeToError =
    maybe (throwError NotFound) pure

convertToServantErr
    :: Error -> ServantErr
convertToServantErr NotFound = err404 { errBody = "not found" }

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

bandName :: Album -> Maybe T.Text
bandName a = fmap (^. name) (a ^. band)

class ToMarkupElem a where
    toMarkupElem :: a -> H.Html

instance ToMarkupElem Album where
    toMarkupElem a = H.div $
        do H.h1 $ toHtml $ a ^. name
           H.img ! HA.src (H.textValue $ fromMaybe "" $ a ^. imageUrl)
           maybe (pure ()) (H.h3 . toHtml) (bandName a)

bandLink :: Band -> Attribute
bandLink b = HA.href $
    H.textValue $
    T.intercalate "/" [ "/api", "bands", T.takeWhile (/= '.') $ b ^. url ]

instance ToMarkupElem Band where
    toMarkupElem b = H.div $
        do H.h1 $ H.a ! bandLink b $ toHtml $ b ^. name
           H.img ! HA.src (H.textValue $ fromMaybe "" $ b ^. imageUrl)
           maybe (pure ()) (H.p . toHtml) (b ^. bio)
           H.div $ forM_ (b ^. albums) toMarkupElem

instance ToMarkup Band where
    toMarkup = baseLayout . toMarkupElem

instance ToMarkupElem a => ToMarkup (SearchResult a) where
    toMarkup (SearchResult xs count) = baseLayout $
        do H.div $ forM_ xs toMarkupElem
           H.p $ H.toHtml count

baseLayout :: H.Html -> MarkupM ()
baseLayout body =
    do H.link ! HA.href "/styles.css" ! HA.rel "stylesheet"
       H.header $ H.h1 "Scaruffi2.0"
       H.body body
       H.footer "this be a footer"

