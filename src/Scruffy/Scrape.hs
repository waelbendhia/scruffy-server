module Scruffy.Scrape ( getVolume, getBand ) where

import           Control.Applicative

import           Data.Maybe
import qualified Data.Set            as S
import qualified Data.Text           as T

import           Network.URI

import qualified Scruffy.Data        as SD

import qualified Text.HTML.Scalpel   as Sc
import           Text.Printf
import           Text.Read

getFullURL :: String -> T.Text -> Maybe T.Text
getFullURL base rel =
    do f <- parseRelativeReference $ T.unpack rel
       b <- parseURI base
       pure $ T.pack $ show $ f `relativeTo` b

-- |Retrieve list of band URLs for volume
getVolume :: Int -> IO (S.Set T.Text)
getVolume v =
    let baseURL = printf "http://scaruffi.com/vol%d/" v
        getBands = mapMaybe (getFullURL baseURL)
            . filter (T.isSuffixOf "html" &&& (/= "index.html")) <$>
            Sc.chroots ("select" Sc.// "option") (Sc.attr "value" "option")
    in S.fromList . fromMaybe [] <$> Sc.scrapeURL baseURL getBands

getBio :: Sc.Scraper T.Text (Maybe T.Text)
getBio = Just <$> bio <|> pure Nothing
  where bio = bioByColor "eebb88"
            <|> bioByColor "#eebb88"
            <|> bioByColor "e6d9ff"

        bioByColor col = T.concat <$>
            Sc.texts ("td" Sc.@: [ "bgcolor" Sc.@= col ])

getAlbums :: Sc.Scraper T.Text [SD.Album]
getAlbums =
    let album text =
            do n <- filter (not . T.null) $ T.strip <$> T.split (== '\n') text
               let name = T.strip $ T.takeWhile ((/= '(') &&& (/= ',')) n
               let year = readMaybe $
                       T.unpack $
                       T.dropEnd 1 $
                       T.drop 1 $
                       T.dropAround ((/= '(') &&& (/= ')')) n
               rating <- maybeToList $
                   readMaybe $
                   T.unpack $
                   T.strip $
                   T.drop 1 $
                   T.takeWhile (/= '/') $
                   T.takeWhileEnd (/= ',') n
               pure $ SD.Album name year rating Nothing Nothing
        as = Sc.chroot ("td" Sc.@: [ "bgcolor" Sc.@= "ffddaa" ]) $
            do text <- Sc.text "td"
               pure $ album text
    in as <|> pure []

getBand :: Sc.URL -> IO (Maybe SD.Band)
getBand bandPage = Sc.scrapeURL bandPage $
    do name <- Sc.text ("center" Sc.// "font")
       bio <- getBio
       albums <- getAlbums <|> pure []
       pure $
           SD.sanitizeBand $
           SD.Band (T.pack bandPage) name bio Nothing [] albums

(&&&) :: (t -> Bool) -> (t -> Bool) -> t -> Bool
(&&&) f1 f2 x = f1 x && f2 x

infixr 3 &&&

