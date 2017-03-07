{-# LANGUAGE OverloadedStrings #-}

import           Data.List   as List
import           Data.Text   as Text
import           Elm         (Spec (Spec), specsToDir, toElmDecoderSource,
                              toElmEncoderSource, toElmTypeSource)
import qualified Elm
import           Servant.API
import           Servant.Elm

import           Api

elmOpts :: ElmOptions
elmOpts = ElmOptions
  { urlPrefix = Static "http://localhost:8000"
  , elmExportOptions = Elm.defaultOptions
  , emptyResponseElmTypes =
      [ Elm.toElmType NoContent
      , Elm.toElmType ()
      ]
  , stringElmTypes =
      [ Elm.toElmType ("" :: String)
      , Elm.toElmType ("" :: Text.Text)
      ]
  }

spec :: Spec
spec =
  Spec ["Api"]
    (defElmImports
    : toElmDecoderSource            (Proxy :: Proxy Item)
    : toElmEncoderSource            (Proxy :: Proxy Item)
    : toElmTypeSource               (Proxy :: Proxy Item)
    : toElmTypeSource               (Proxy :: Proxy NoContent)
    : generateElmForAPIWith elmOpts (Proxy :: Proxy Api)
    )

main :: IO ()
main = specsToDir [spec] "client"
