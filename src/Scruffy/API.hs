{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Scruffy.API ( API, api, server ) where

import           Control.Monad.Reader

import           Data.Text            as T

import           Scruffy.Data
import           Scruffy.Service

import           Servant

type AlbumsEndpoint = QueryParam "name" T.Text
    :> QueryParam "page" Int :> QueryParam "itemsPerPage" Int
    :> Get '[JSON] (SearchResult Album)

type BandsEndpoint = QueryParam "name" T.Text
    :> QueryParam "page" Int :> QueryParam "itemsPerPage" Int
    :> Get '[JSON] (SearchResult Band)

type BandEndpoint =
    Capture "volume" T.Text :> Capture "url" T.Text :> Get '[JSON] Band

type BandsAPI = "bands"
    :> (BandsEndpoint :<|> BandEndpoint)

type AlbumsAPI = "albums" :> AlbumsEndpoint

type API = BandsAPI :<|> AlbumsAPI

api :: Proxy API
api = Proxy

bandsServer :: Service a => a -> Server BandsAPI
bandsServer svc =
    let bandsEndpoint mN mP mIPP =
            liftIO $ searchBands svc $ SearchRequest mP mIPP mN
        bandEndpoint vol path =
            do b <- liftIO $ getBand svc $ T.concat [ vol, "/", path, ".html" ]
               maybe (throwError err404 { errBody = "band not found" }) pure b
    in bandsEndpoint :<|> bandEndpoint

albumsServer :: Service a => a -> Server AlbumsAPI
albumsServer svc =
    let albumsEndpoint aN mP mIPP = liftIO $
            searchAlbums svc $
            AlbumSearchRequest { getAlbumSearchBase = SearchRequest mP mIPP aN
                               , getAlbumSearchYearLower = Nothing
                               , getAlbumSearchYearUpper = Nothing
                               , getAlbumSearchIncludeUnknownYear = Nothing
                               , getAlbumSearchRatingLower = Nothing
                               , getAlbumSearchRatingUpper = Nothing
                               , getAlbumSearchSortColumn = Nothing
                               }
    in albumsEndpoint

server :: Service a => a -> Server API
server svc = bandsServer svc :<|> albumsServer svc
