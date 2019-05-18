module Scruffy.Service
    ( Service(..)
    , SearchRequest(..)
    , AlbumSearchRequest(..)
    , BasicService(..)
    ) where

import           Data.Maybe
import           Data.Text          as T

import qualified Scruffy.Data       as SD
import qualified Scruffy.Repository as R

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
                       , getAlbumSearchSortColumn :: Maybe R.SortColumn
                       }

convertASR :: AlbumSearchRequest -> R.AlbumSearchRequest
convertASR (AlbumSearchRequest base yL yU yUn rL rU sC) =
    R.AlbumSearchRequest (convertSR base)
                         ( fromMaybe 0 yL
                         , fromMaybe 4000 yU
                         , fromMaybe True yUn
                         )
                         (fromMaybe 0 rL, fromMaybe 10 rU)
                         (fromMaybe R.Rating sC)

class Service a where
    searchBands :: a -> SearchRequest -> IO (SD.SearchResult SD.Band)
    searchAlbums :: a -> AlbumSearchRequest -> IO (SD.SearchResult SD.Album)
    getBand :: a -> T.Text -> IO (Maybe SD.Band)

newtype R.Repository a => BasicService a =
    BasicService { unRepo :: a }

instance R.Repository a => Service (BasicService a) where
    searchAlbums svc req = uncurry SD.SearchResult <$>
        R.searchAlbums (unRepo svc) (convertASR req)

    searchBands svc req = uncurry SD.SearchResult <$>
        R.searchBands (unRepo svc) (convertSR req)

    getBand svc = R.getBand (unRepo svc)
