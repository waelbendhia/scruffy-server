{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Scruffy.API ( API, api, server ) where

import           Clay

import           Control.Monad.Reader

import           Data.Text            as T

import           Scruffy.Css
import           Scruffy.Data
import qualified Scruffy.Repository   as R
import           Scruffy.Service

import           Servant
import           Servant.HTML.Blaze

type AlbumsEndpoint = QueryParam "name" T.Text :> QueryParam "page" Int
    :> QueryParam "itemsPerPage" Int :> QueryParam "ratingLower" Double
    :> QueryParam "ratingUpper" Double :> QueryParam "yearLower" Int
    :> QueryParam "yearUpper" Int :> QueryParam "includeUnknown" Bool
    :> QueryParam "sortBy" (R.Sorting R.SortColumn)
    :> Get '[JSON, HTML] (SearchResult Album)

type BandsEndpoint = QueryParam "name" T.Text
    :> QueryParam "page" Int :> QueryParam "itemsPerPage" Int
    :> Get '[JSON, HTML] (SearchResult Band)

type BandEndpoint =
    Capture "volume" T.Text :> Capture "url" T.Text :> Get '[JSON, HTML] Band

type BandsAPI = "bands"
    :> (BandsEndpoint :<|> BandEndpoint)

type AlbumsAPI = "albums" :> AlbumsEndpoint

type DynamicAPI = "api"
    :> (BandsAPI :<|> AlbumsAPI)

type StaticAPI = "styles.css" :> Get '[CSS] Css

type API = DynamicAPI :<|> StaticAPI

api :: Proxy API
api = Proxy

bandsServer :: Service a => a -> Server BandsAPI
bandsServer svc =
    let bandsEndpoint mN mP mIPP =
            liftIO $ searchBands svc $ SearchRequest mP mIPP mN
        bandEndpoint vol path =
            do res <- liftIO $
                   getBand svc $
                   T.concat [ vol, "/", path, ".html" ]
               maybe (throwError err404 { errBody = "band not found" })
                     pure
                     res
    in bandsEndpoint :<|> bandEndpoint

albumsServer :: Service a => a -> Server AlbumsAPI
albumsServer svc =
    let albumsEndpoint aN mP mIPP mRL mRU mYL mYU mIU sColumn = liftIO $
            searchAlbums svc $
            AlbumSearchRequest { getAlbumSearchBase = SearchRequest mP mIPP aN
                               , getAlbumSearchYearLower = mYL
                               , getAlbumSearchYearUpper = mYU
                               , getAlbumSearchIncludeUnknownYear = mIU
                               , getAlbumSearchRatingLower = mRL
                               , getAlbumSearchRatingUpper = mRU
                               , getAlbumSearchSortColumn = sColumn
                               }
    in albumsEndpoint

dynamicServer :: Service a => a -> Server DynamicAPI
dynamicServer svc = bandsServer svc :<|> albumsServer svc

staticServer :: Server StaticAPI
staticServer = pure stylesheet

server :: Service a => a -> Server API
server svc = dynamicServer svc :<|> staticServer

