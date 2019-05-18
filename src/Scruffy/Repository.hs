module Scruffy.Repository
    ( Repository(..)
    , PGRepo(..)
    , connectPGRepo
    , SortColumn(..)
    , SearchRequest(..)
    , AlbumSearchRequest(..)
    ) where

import           Control.Lens

import           Data.Maybe
import           Data.Pool
import           Data.String
import           Data.Text                          as T

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField

import qualified Scruffy.Data                       as SD

class Repository a where
    searchAlbums :: a -> AlbumSearchRequest -> IO ([SD.Album], Int)
    searchBands :: a -> SearchRequest -> IO ([SD.Band], Int)
    getBand :: a -> T.Text -> IO (Maybe SD.Band)

newtype PGRepo = PGRepo { unRepo :: Pool Connection }

newtype PGBand = PGBand { unPGBand :: SD.Band }

newtype PGAlbum = PGAlbum { unPGAlbum :: SD.Album }

data SortColumn = Rating | Year | AlbumName | BandName
    deriving Show

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

instance FromRow PGBand where
    fromRow =
        do partialUrl <- field
           n <- field
           b <- field
           i <- field
           pure $ PGBand $ SD.Band partialUrl n b i [] []

instance FromRow PGAlbum where
    fromRow =
        do n <- field
           y <- field
           r <- field
           i <- field
           bn <- field
           bu <- field
           pure $
               PGAlbum $
               SD.Album n y r (Just $ SD.Band bu bn Nothing Nothing [] []) i

instance ToField SortColumn where
    toField c = Escape $
        case c of
            Rating -> "rating"
            Year -> "year"
            AlbumName -> "albumnname"
            BandName -> "bandname"

instance IsString SortColumn where
    fromString "year" = Year
    fromString "albumName" = AlbumName
    fromString "bandName" = BandName
    fromString _ = Rating

withTx :: PGRepo -> (Connection -> IO b) -> IO b
withTx repo action =
    withResource (unRepo repo) $ \conn -> conn `withTransaction` action conn

instance Repository PGRepo where
    searchAlbums repo
                 AlbumSearchRequest{ getAlbumSearchRating = (rLower, rUpper)
                                   , getAlbumSearchYear =
                                         (yLower, yUpper, includeUnknown)
                                   , getAlbumSearchSortColumn = sColumn
                                   , getAlbumSearchBase =
                                         SearchRequest p ipp name
                                   } =
        let selectParams =
                ( rLower
                , rUpper
                , yLower
                , yUpper
                , includeUnknown
                , name
                , name
                , name
                , sColumn
                , sColumn
                , sColumn
                , sColumn
                , ipp
                , ipp * p
                )
            countParams =
                ( rLower
                , rUpper
                , yLower
                , yUpper
                , includeUnknown
                , name
                , name
                , name
                )
            searchAlbums' conn =
                do as <- query conn selectAlbumsQuery selectParams
                   [[c]] <- query conn countAlbumsQuery countParams
                   pure (unPGAlbum <$> as, c)
        in repo `withTx` searchAlbums'

    searchBands repo (SearchRequest p ipp n) =
        let searchBands' conn =
                do bs <- query conn selectBandsQuery (n, ipp, ipp * p)
                   [[c]] <- query conn countBandsQuery [ n ]
                   pure (unPGBand <$> bs, c)
        in repo `withTx` searchBands'

    getBand repo path =
        let getBand' conn =
                do b <- query conn selectBandQuery [ path ]
                   sequence $ listToMaybe $ setAlbums conn . unPGBand <$> b
        in repo `withTx` getBand'

getAlbumsByBand :: Connection -> T.Text -> IO [SD.Album]
getAlbumsByBand conn bandURL = fmap unPGAlbum <$>
    query conn
          "SELECT name, year, rating, imageurl, null, null \
          \FROM albums WHERE band = ?"
          [ bandURL ]

setAlbums :: Connection -> SD.Band -> IO SD.Band
setAlbums conn b = flip (set SD.albums) b <$>
    getAlbumsByBand conn (view SD.url b)

selectAlbumsQuery :: Query
selectAlbumsQuery =
    "SELECT \
      \a.name, \
      \a.year, \
      \a.rating, \
      \a.imageurl, \
      \b.name, \
      \b.partialurl \
    \FROM albums a INNER JOIN bands b ON b.partialURL = a.band \
    \WHERE \
      \a.rating BETWEEN ? AND ? AND \
      \(a.year BETWEEN ? AND ? AND a.year != 0 OR (a.year = 0 AND ?)) AND \
      \(? = '' OR lower(a.name) ~ lower(?) OR lower(b.name) ~ lower(?)) \
      \ORDER BY \
        \CASE WHEN ? = 'rating'    THEN a.rating END DESC, \
        \CASE WHEN ? = 'year'      THEN a.year END DESC, \
        \CASE WHEN ? = 'albumname' THEN a.name END DESC, \
        \CASE WHEN ? = 'bandname'  THEN b.name END DESC \
      \LIMIT ? OFFSET ?"

countAlbumsQuery :: Query
countAlbumsQuery =
    "SELECT count(*) \
    \FROM albums a INNER JOIN bands b ON b.partialURL = a.band \
    \WHERE \
      \a.rating BETWEEN ? AND ? AND \
      \(a.year BETWEEN ? AND ? AND a.year != 0 OR (a.year = 0 AND ?)) AND \
      \(? = '' OR lower(a.name) ~ lower(?) OR lower(b.name) ~ lower(?))"

selectBandsQuery :: Query
selectBandsQuery =
    "SELECT partialurl, name, null, imageurl \
    \FROM bands \
    \WHERE lower(name) ~ lower(?) \
    \ORDER BY name LIMIT ? OFFSET ?"

countBandsQuery :: Query
countBandsQuery = "SELECT count(*) FROM bands WHERE lower(name) ~ lower(?)"

selectBandQuery :: Query
selectBandQuery =
    "SELECT partialurl, name, bio, imageurl FROM bands where partialurl = ?"


