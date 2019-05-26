{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Scruffy.Css ( CSS, stylesheet ) where

import           Clay

import qualified Data.Text               as T
import           Data.Text.Lazy.Encoding ( encodeUtf8 )
import           Data.Typeable           ( Typeable )

import qualified Network.HTTP.Media      as M

import           Servant.API             ( Accept(..), MimeRender(..) )

data CSS
    deriving Typeable

instance Accept CSS where
    contentType _ = "text" M.// "css" M./: ("charset", "utf-8")

instance MimeRender CSS Css where
    mimeRender _ = encodeUtf8 . renderWith compact []

fontImports :: Css
fontImports = importUrl $
    T.concat [ "https://fonts.googleapis.com/css?family="
             , "Libre+Baskerville:400,700|"
             , "Work+Sans:300,400,700&subset=latin-ext"
             ]

scruffyBlack :: Color
scruffyBlack = rgb 26 26 26

bodyFont :: Css
bodyFont = fontFamily [ "Work Sans" ] [ sansSerif ]

headerFont :: Css
headerFont = fontFamily [ "Libre Baskerville" ] [ serif ]

headingsFont :: Css
headingsFont =
    mapM_ (? do headerFont
                margin (px 0) (px 0) (px 0) (px 0))
          [ h1, h2, h3, h4, h5, h6 ]

stylesheet :: Css
stylesheet =
    do fontImports
       headingsFont
       body
           ? do margin (px 0) (px 0) (px 0) (px 0)
                bodyFont
       header
           ? do alignItems center
                justifyContent center
                flexDirection row
                display flex
                height (px 60)
                position sticky
                top (px 0)
                backgroundColor scruffyBlack
                color white
