{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Scruffy.Repository
    ( Repository(..)
    , PGRepo(..)
    , connectPGRepo
    , Sorting(..)
    , SortOrder(..)
    , SortColumn(..)
    , SearchRequest(..)
    , AlbumSearchRequest(..)
    ) where

import           Control.Arrow
import           Control.Lens               as L

import           Data.Int
import           Data.Maybe
import           Data.Pool
import           Data.Profunctor.Product
import           Data.Text                  as T

import           Database.PostgreSQL.Simple

import           Opaleye                    as O

import qualified Scruffy.Data               as SD

import           Servant.API

type BandRow' text maybeText = (text, text, maybeText, maybeText)

type BandRow = BandRow' (Field SqlText) (FieldNullable SqlText)

type BandRowR = BandRow' Text (Maybe Text)

convertRowToBand :: BandRowR -> SD.Band
convertRowToBand (url, name, bio, imageURL) =
    SD.Band url name bio imageURL [] []

bandTable :: Table BandRow BandRow
bandTable =
    table "bands"
          (p4 ( tableField "partialurl"
              , tableField "name"
              , tableField "bio"
              , tableField "imageurl"
              ))

type AlbumRow' text maybeText maybeInt float =
    (text, maybeInt, float, text, maybeText)

type AlbumRow =
    AlbumRow' (Field SqlText) (FieldNullable SqlText) (FieldNullable SqlInt4) (Field SqlFloat8)

type AlbumRowR = AlbumRow' Text (Maybe Text) (Maybe Int) Double

convertRowToAlbum :: AlbumRowR -> SD.Album
convertRowToAlbum (name, year, rating, bandURL, imageURL) =
    SD.Album name
             year
             rating
             (Just $ SD.Band bandURL "" Nothing Nothing [] [])
             imageURL

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

(.=~) :: Column PGText -> Text -> Column PGBool
(.=~) a b = a `O.ilike` sqlStrictText (T.concat [ "%", b, "%" ])

nullInt4 :: Int -> Column (Nullable PGInt4)
nullInt4 = toNullable . sqlInt4

between :: PGOrd a => Column a -> Column a -> Column a -> Column PGBool
between x a b = x .>= a .&& x .<= b

paginate :: Int -> Int -> Select a -> Select a
paginate p ipp = limit ipp
    . offset (p * ipp)

extractCount :: Connection -> Select (Column a) -> IO Int
extractCount c q = maybe 0 (fromIntegral :: Int64 -> Int) . listToMaybe <$>
    runQuery c (aggregate O.count q)

class Repository a where
    searchAlbums :: a -> AlbumSearchRequest -> IO ([SD.Album], Int)
    searchBands :: a -> SearchRequest -> IO ([SD.Band], Int)
    getBand :: a -> T.Text -> IO (Maybe SD.Band)

newtype PGRepo = PGRepo { unRepo :: Pool Connection }

data Sorting a = Sorting a SortOrder
    deriving Show

data SortOrder = Asc | Desc
    deriving Show

data SortColumn = Rating | Year | AlbumName | BandName
    deriving Show

instance FromHttpApiData a => FromHttpApiData (Sorting a) where
    parseQueryParam s = case T.splitOn "," s of
        [column, order] ->
            Sorting <$> parseQueryParam column <*> parseQueryParam order
        [column] -> (`Sorting` Desc) <$> parseQueryParam column
        _ -> Left $ T.concat [ "could not parse sorter order ", s ]

instance FromHttpApiData SortOrder where
    parseQueryParam "desc" = Right Desc
    parseQueryParam "asc" = Right Asc
    parseQueryParam x = Left $
        T.concat [ "unknown order direction '"
                 , x
                 , "'. Options are: asc, desc"
                 ]

instance FromHttpApiData SortColumn where
    parseQueryParam "year" = Right Year
    parseQueryParam "albumName" = Right AlbumName
    parseQueryParam "bandName" = Right BandName
    parseQueryParam "rating" = Right Rating
    parseQueryParam x = Left $
        T.concat ([ "unknown column '", x, "'. Options are: " ]
                  ++ [ intercalate ", "
                                   [ "year"
                                   , "albumName"
                                   , "bandName"
                                   , "rating"
                                   ]
                     ])

data SearchRequest =
    SearchRequest { getPage         :: Int
                  , getItemsPerPage :: Int
                  , getSearchTerm   :: T.Text
                  }

data AlbumSearchRequest =
    AlbumSearchRequest { getAlbumSearchBase    :: SearchRequest
                       , getAlbumSearchYear    :: (Int, Int, Bool)
                       , getAlbumSearchRating  :: (Double, Double)
                       , getAlbumSearchSorting :: Sorting SortColumn
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
                                   , getAlbumSearchSorting = sorting
                                   , getAlbumSearchBase = SearchRequest p ipp n
                                   } =
        let baseQuery = proc () ->
                do row@(albumName, year, rating, albumBand, _)
                       <- albumSelect -< ()
                   (bandURL, bandName, _, _) <- bandSelect -< ()
                   restrict -< bandURL .== albumBand
                   restrict -< bandName .=~ n .|| albumName .=~ n
                   restrict
                       -< between rating (sqlDouble rLower) (sqlDouble rUpper)
                   restrict
                       -< between year (nullInt4 yLower) (nullInt4 yUpper)
                       .|| (isNull year .&& sqlBool includeUnknown)
                   returnA -< (row , bandName)
            sorter =
                let Sorting column order = sorting
                    direction Asc = O.asc
                    direction Desc = O.desc
                in orderBy $
                   case column of
                       AlbumName -> direction order (^. _1 . _1)
                       Year -> direction order (^. _1 . _2)
                       Rating -> direction order (^. _1 . _3)
                       BandName -> direction order (^. _2)
            convertResult (albumRow, bandName) = convertRowToAlbum albumRow
                & SD.band . _Just . SD.name .~ bandName
            search conn = fmap convertResult <$>
                runQuery conn (paginate p ipp $ sorter baseQuery)
            count' c = extractCount c $ (^. _1 . _1) <$> baseQuery
        in repo `withConn` \conn -> (,) <$> search conn <*> count' conn

    searchBands repo (SearchRequest p ipp n) =
        let baseQuery = proc () ->
                do row@(_, name, _, _) <- bandSelect -< ()
                   restrict -< name .=~ n
                   returnA -< row & _3 .~ (O.null :: Column (Nullable SqlText))
            search c = fmap convertRowToBand <$>
                runQuery c (paginate p ipp $ orderBy (asc (^. _2)) baseQuery)
            count' c = extractCount c $ (^. _1) <$> baseQuery
        in repo `withConn` \c -> (,) <$> search c <*> count' c

    getBand repo partialURL =
        let bandQuery = proc () ->
                do row <- bandSelect -< ()
                   restrict -< row ^. _1 .== pgStrictText partialURL
                   returnA -< row
            albumsQuery = proc () ->
                do row@(_, _, _, band, _) <- albumSelect -< ()
                   restrict -< band .== pgStrictText partialURL
                   returnA -< row 
            populateAlbums c band =
                do asR <- runQuery c albumsQuery
                   let as = (SD.band .~ Nothing) . convertRowToAlbum <$> asR
                   pure $ band & SD.albums .~ as
        in repo
           `withConn` \c -> runQuery c bandQuery
           >>= (mapM (populateAlbums c . convertRowToBand) . listToMaybe)


