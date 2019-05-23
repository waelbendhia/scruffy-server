{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Scruffy.Repository
    ( Repository(..)
    , PGRepo(..)
    , connectPGRepo
    , SortColumn(..)
    , SearchRequest(..)
    , AlbumSearchRequest(..)
    ) where

import           Control.Arrow

import           Data.Int
import           Data.Maybe
import           Data.Pool
import           Data.Profunctor.Product
import           Data.String
import           Data.Text                  as T

import           Database.PostgreSQL.Simple

import           Opaleye                    as O

import qualified Scruffy.Data               as SD

type BandRow =
    ( Field SqlText
    , Field SqlText
    , FieldNullable SqlText
    , FieldNullable SqlText
    )

bandTable :: Table BandRow BandRow
bandTable =
    table "bands"
          (p4 ( tableField "partialurl"
              , tableField "name"
              , tableField "bio"
              , tableField "imageurl"
              ))

type AlbumRow =
    ( Field SqlText
    , FieldNullable SqlInt4
    , Field SqlFloat8
    , Field SqlText
    , FieldNullable SqlText
    )

albumTable :: Table AlbumRow AlbumRow
albumTable =
    table "albums"
          (p5 ( tableField "name"
              , tableField "year"
              , tableField "rating"
              , tableField "band"
              , tableField "imageurl"
              ))

bandSelect :: Select BandRow
bandSelect = selectTable bandTable

albumSelect :: Select AlbumRow
albumSelect = selectTable albumTable

restrictName :: Text -> QueryArr (Column PGText) ()
restrictName n = proc name -> restrict -< name
    `ilike` sqlStrictText (T.concat [ "%", n, "%" ])

paginate :: Int -> Int -> Select a -> Select a
paginate p ipp = limit ipp
    . offset (p * ipp)

aggCount :: Select (Column a)
         -> Select (Column SqlInt8)
aggCount = aggregate O.count

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

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

fst6 :: (a, b, c, d, e, f) -> a
fst6 (x, _, _, _, _, _) = x

snd6 :: (a, b, c, d, e, f) -> b
snd6 (_, x, _, _, _, _) = x

trd6 :: (a, b, c, d, e, f) -> c
trd6 (_, _, x, _, _, _) = x

sxt6 :: (a, b, c, d, e, f) -> f
sxt6 (_, _, _, _, _, x) = x

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

instance Repository PGRepo where
    searchAlbums repo
                 AlbumSearchRequest{ getAlbumSearchRating = (rLower, rUpper)
                                   , getAlbumSearchYear =
                                         (yLower, yUpper, includeUnknown)
                                   , getAlbumSearchSortColumn = sColumn
                                   , getAlbumSearchBase =
                                         SearchRequest p ipp name
                                   } =
        let searchTerm = sqlStrictText (T.concat [ "%", name, "%" ])
            baseQuery = proc () ->
                do (albumName, y, rating, band, i) <- albumSelect -< ()
                   (bandURL, bandName, _, _) <- bandSelect -< ()
                   restrict -< bandURL .== band
                   restrict -< bandName `ilike` searchTerm .|| albumName
                       `ilike` searchTerm
                   restrict -< rating .>= sqlDouble rLower .&& rating
                       .<= sqlDouble rUpper
                   let year = ifThenElse (matchNullable (sqlBool False)
                                                        (.== sqlInt4 0)
                                                        y)
                                         O.null
                                         y
                   restrict
                       -< matchNullable (sqlBool False)
                                        (\year' -> year' .>= sqlInt4 yLower
                                         .&& year'
                                         .<= sqlInt4 yUpper)
                                        year
                       .|| (isNull year .&& sqlBool includeUnknown)
                   returnA -< (albumName, year, rating, band, i, bandName)
            countQuery = aggCount $ fst6 <$> baseQuery
            sorter = orderBy $
                case sColumn of
                    AlbumName -> asc fst6
                    Year -> desc snd6
                    Rating -> desc trd6
                    BandName -> asc sxt6
            searchQuery = paginate p ipp $ sorter baseQuery
            conv :: Int64 -> Int
            conv = fromIntegral
        in repo
           `withConn` \conn ->
           do as <- runQuery conn searchQuery
              xs <- runQuery conn countQuery
              pure ( (\(albumName, year, rating, bandURL, imageURL, bandName) ->
                      SD.Album albumName
                               year
                               rating
                               (Just $
                                SD.Band bandURL bandName Nothing Nothing [] [])
                               imageURL) <$>
                         as
                   , maybe 0 conv $ listToMaybe xs
                   )

    searchBands repo (SearchRequest p ipp n) =
        let baseQuery = proc () ->
                do (url, name, _, image) <- bandSelect -< ()
                   restrictName n -< name
                   returnA -< (url, name, image)
            countQuery = aggCount $ fst3 <$> baseQuery
            searchQuery = paginate p ipp $
                orderBy (asc snd3) baseQuery
            conv :: Int64 -> Int
            conv = fromIntegral
        in repo
           `withConn` \conn ->
           do bs <- runQuery conn searchQuery
              xs <- runQuery conn countQuery
              pure ( (\(url, name, image) ->
                      SD.Band url name Nothing image [] []) <$>
                         bs
                   , maybe 0 conv $ listToMaybe xs
                   )

    getBand repo partialURL =
        let bandQuery = proc () ->
                do row@(url, _, _, _) <- bandSelect -< ()
                   restrict -< url .== pgStrictText partialURL
                   returnA -< row
            albumsQuery = proc () ->
                do (n, y, r, band, i) <- albumSelect -< ()
                   restrict -< band .== pgStrictText partialURL
                   let year = ifThenElse (matchNullable (sqlBool False)
                                                        (.== sqlInt4 0)
                                                        y)
                                         O.null
                                         y
                   returnA -< (n, year, r, i)
        in repo
           `withConn` \conn ->
           do b <- runQuery conn bandQuery
              mapM (\(url, name, bio, imageURL) ->
                    do as <- runQuery conn albumsQuery
                       pure $
                           SD.Band url
                                   name
                                   bio
                                   imageURL
                                   []
                                   ((\(n, y, r, i) ->
                                     SD.Album n y r Nothing i) <$>
                                    as))
                   (listToMaybe b)

