{-# LANGUAGE UndecidableInstances #-}

module Scruffy.Service
    ( Service(..)
    , SearchRequest(..)
    , AlbumSearchRequest(..)
    , ServiceT(..)
    ) where

import           Control.Monad.Except

import           Data.Maybe
import           Data.Text            as T

import qualified Scruffy.Data         as SD
import qualified Scruffy.Repository   as R

data SearchRequest =
    SearchRequest { page         :: Maybe Int
                  , itemsPerPage :: Maybe Int
                  , name         :: Maybe T.Text
                  }

convertSR :: SearchRequest -> R.SearchRequest
convertSR (SearchRequest p ipp n) =
    R.SearchRequest (max 0 $ fromMaybe 0 p)
                    (max 1 $ min 50 $ fromMaybe 10 ipp)
                    (fromMaybe "" n)

data AlbumSearchRequest =
    AlbumSearchRequest { getAlbumSearchBase :: SearchRequest
                       , getAlbumSearchYearLower :: Maybe Int
                       , getAlbumSearchYearUpper :: Maybe Int
                       , getAlbumSearchIncludeUnknownYear :: Maybe Bool
                       , getAlbumSearchRatingLower :: Maybe Double
                       , getAlbumSearchRatingUpper :: Maybe Double
                       , getAlbumSearchSortColumn
                             :: Maybe (R.Sorting R.SortColumn)
                       }

convertASR :: AlbumSearchRequest -> R.AlbumSearchRequest
convertASR (AlbumSearchRequest base yL yU yUn rL rU sC) =
    R.AlbumSearchRequest (convertSR base)
                         ( fromMaybe 0 yL
                         , fromMaybe 4000 yU
                         , fromMaybe True yUn
                         )
                         (fromMaybe 0 rL, fromMaybe 10 rU)
                         (fromMaybe (R.Sorting R.Rating R.Desc) sC)

class Service m where
    searchBands :: SearchRequest -> m (SD.SearchResult SD.Band)
    searchAlbums :: AlbumSearchRequest -> m (SD.SearchResult SD.Album)
    getBand :: T.Text -> m (Maybe SD.Band)

newtype (Monad m, MonadIO m, R.Repository m) => ServiceT m a =
    ServiceT { runServiceT :: m a }
    deriving ( Functor, Applicative, Monad, MonadIO, R.Repository )

instance (Monad m, R.Repository m) => Service (ServiceT m) where
    searchAlbums req = uncurry SD.SearchResult <$>
        R.searchAlbums (convertASR req)

    searchBands req = uncurry SD.SearchResult <$>
        R.searchBands (convertSR req)

    getBand = R.getBand
