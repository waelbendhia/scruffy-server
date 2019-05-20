{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Scruffy.Repository
    ( Repository(..)
    , PGRepo(..)
    , connectPGRepo
    , SortColumn(..)
    , SearchRequest(..)
    , AlbumSearchRequest(..)
    ) where

import           Control.Lens           as L

import           Data.Pool
import           Data.String
import           Data.Text              as T

import           Database.Beam
import           Database.Beam.Postgres

import qualified Scruffy.Data           as SD

data BandT f =
    Band { _bandUrl      :: Columnar f Text
         , _bandName     :: Columnar f Text
         , _bandBio      :: Columnar f (Maybe Text)
         , _bandImageUrl :: Columnar f (Maybe Text)
         }
    deriving Generic

instance Beamable BandT

instance Table BandT where
    data PrimaryKey BandT f = BandId (Columnar f Text)
            deriving ( Generic, Beamable )

    primaryKey = BandId . _bandUrl

data AlbumT f =
    Album { _albumName     :: Columnar f Text
          , _albumYear     :: Columnar f (Maybe Int)
          , _albumRating   :: Columnar f Double
          , _albumBand     :: Columnar f Text
          , _albumImageUrl :: Columnar f (Maybe Text)
          }
    deriving Generic

type Album = AlbumT Identity

convertAlbum :: Album -> SD.Album
convertAlbum a =
    SD.Album (_albumName a)
             (_albumYear a)
             (_albumRating a)
             (Just $ SD.Band (_albumBand a) "" Nothing Nothing [] [])
             (_albumImageUrl a)

instance Beamable AlbumT

instance Table AlbumT where
    data PrimaryKey AlbumT f = AlbumId (Columnar f Text)
            deriving ( Generic, Beamable )

    primaryKey = AlbumId . _albumName

data ScaruffiDb f =
    ScaruffiDb { _scaruffiBands  :: f (TableEntity BandT)
               , _scaruffiAlbums :: f (TableEntity AlbumT)
               }
    deriving ( Generic, Database be )

scaruffiDb :: DatabaseSettings be ScaruffiDb
scaruffiDb =
    let albumModification =
            tableModification { _albumBand     = fieldNamed "band"
                              , _albumImageUrl = fieldNamed "imageurl"
                              }
        bandModification =
            tableModification { _bandUrl      = fieldNamed "partialurl"
                              , _bandImageUrl = fieldNamed "imageurl"
                              }
    in withDbModification defaultDbSettings $
       dbModification { _scaruffiAlbums = setEntityName "albums"
                            <> modifyTableFields albumModification
                      , _scaruffiBands  = setEntityName "bands"
                            <> modifyTableFields bandModification
                      }

class Repository a where
    searchAlbums :: a -> AlbumSearchRequest -> IO ([SD.Album], Int)
    searchBands :: a -> SearchRequest -> IO ([SD.Band], Int)
    getBand :: a -> T.Text -> IO (Maybe SD.Band)

newtype PGRepo = PGRepo { unRepo :: Pool Connection }

data SortColumn = Rating | Year | AlbumName | BandName
    deriving Show

instance IsString SortColumn where
    fromString "year" = Year
    fromString "albumName" = AlbumName
    fromString "bandName" = BandName
    fromString _ = Rating

data SearchRequest =
    SearchRequest { getPage         :: Int
                  , getItemsPerPage :: Int
                  , getSearchTerm   :: T.Text
                  }

data AlbumSearchRequest =
    AlbumSearchRequest { getAlbumSearchBase       :: SearchRequest
                       , getAlbumSearchYear       :: (Int, Int, Bool)
                       , getAlbumSearchRating     :: (Double, Double)
                       , getAlbumSearchSortColumn :: SortColumn
                       }

connectPGRepo :: ConnectInfo -> IO PGRepo
connectPGRepo info = PGRepo <$>
    createPool (connect info) close 1 10 10

withConn :: PGRepo -> (Connection -> IO b) -> IO b
withConn repo = withResource (unRepo repo)

instance Repository PGRepo where
    searchAlbums repo
                 AlbumSearchRequest{ getAlbumSearchRating = (rLower, rUpper)
                                   , getAlbumSearchYear =
                                         (yLower, yUpper, includeUnknown)
                                   , getAlbumSearchSortColumn = sColumn
                                   , getAlbumSearchBase =
                                         SearchRequest p ipp name
                                   } =
        let similar_ c t
                | T.null t = val_ True
                | otherwise = lower_ c
                    `like_` val_ (T.concat [ "%", T.toLower t, "%" ])
            sortQuery = case sColumn of
                Rating -> orderBy_ (desc_ . _albumRating . fst)
                AlbumName -> orderBy_ (asc_ . _albumName . fst)
                BandName -> orderBy_ (asc_ . snd)
                Year -> orderBy_ (desc_ . _albumYear . fst)
            albumsQuery =
                do album <- all_ $ _scaruffiAlbums scaruffiDb
                   band <- all_ $ _scaruffiBands scaruffiDb
                   guard_ (between_ (_albumRating album)
                                    (val_ rLower)
                                    (val_ rUpper))
                   guard_ (maybe_ (val_ includeUnknown)
                                  (\y -> between_ y (val_ yLower) (val_ yUpper))
                                  (_albumYear album))
                   guard_ (_bandName band `similar_` name ||. _albumName album
                           `similar_` name)
                   guard_ (_bandUrl band ==. _albumBand album)
                   pure (album, _bandName band)
            countQuery =
                do album <- all_ $ _scaruffiAlbums scaruffiDb
                   band <- all_ $ _scaruffiBands scaruffiDb
                   guard_ (between_ (_albumRating album)
                                    (val_ rLower)
                                    (val_ rUpper))
                   guard_ (maybe_ (val_ includeUnknown)
                                  (\y -> between_ y (val_ yLower) (val_ yUpper))
                                  (_albumYear album))
                   guard_ (_bandName band `similar_` name ||. _albumName album
                           `similar_` name)
                   guard_ (_bandUrl band ==. _albumBand album)
                   pure (album, _bandName band)
        in repo
           `withConn` \conn -> runBeamPostgres conn $
           do albums <- runSelectReturningList $
                  select $
                  limit_ (fromIntegral ipp) $
                  offset_ (fromIntegral $ p * ipp) $
                  sortQuery albumsQuery
              Just c <- runSelectReturningOne $
                  select $
                  aggregate_ (\_ -> as_ @Int countAll_) $
                  countQuery
              pure ( (\(a, b) ->
                      L.set (SD.band . _Just . SD.name) b (convertAlbum a)) <$>
                         albums
                   , c
                   )

    searchBands repo (SearchRequest p ipp n) =
        let similar_ c t
                | T.null t = val_ True
                | otherwise = lower_ c
                    `like_` val_ (T.concat [ "%", T.toLower t, "%" ])
            bandsQuery =
                do b <- limit_ (fromIntegral ipp) $
                       offset_ (fromIntegral $ ipp * p) $
                       filter_ ((`similar_` n) . _bandName) $
                       orderBy_ (asc_ . _bandName) $
                       all_ $
                       _scaruffiBands scaruffiDb
                   pure (_bandName b, _bandUrl b, _bandImageUrl b)
            countQuery = aggregate_ (const $ as_ @Int countAll_) $
                filter_ ((`similar_` n) . _bandName) $
                all_ $
                _scaruffiBands scaruffiDb
            thrupleToBand (bn, url, i) = SD.Band bn url Nothing i [] []
        in repo
           `withConn` \conn -> runBeamPostgres conn $
           do bs <- runSelectReturningList $ select bandsQuery
              Just c <- runSelectReturningOne $ select countQuery
              pure (thrupleToBand <$> bs, c)

    getBand _ _ = pure Nothing

