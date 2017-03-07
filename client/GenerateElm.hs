{-# LANGUAGE OverloadedStrings #-}

import           Data.List   as List
import           Data.Text   as Text
import           Elm         (Spec (Spec), specsToDir, toElmDecoderSource,
                              toElmEncoderSource, toElmTypeSource)
import qualified Elm
import           Servant.API
import           Servant.Elm

import           Api

spec :: Spec
spec =
  Spec ["Api"]
    (defElmImports
    : toElmDecoderSource            (Proxy :: Proxy Item)
    : toElmEncoderSource            (Proxy :: Proxy Item)
    : toElmTypeSource               (Proxy :: Proxy Item)
    : toElmTypeSource               (Proxy :: Proxy NoContent)
    : generateElmForAPI             (Proxy :: Proxy Api)
    )

main :: IO ()
main = specsToDir [spec] "client"
