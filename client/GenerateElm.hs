{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

import           Data.List
import           Data.Text               hiding ( intercalate
                                                , map
                                                )
import           Servant.Elm                    ( DefineElm(DefineElm)
                                                , Proxy(Proxy)
                                                , defaultOptions
                                                , defElmImports
                                                , defElmOptions
                                                , deriveBoth
                                                , generateElmModuleWith
                                                )
import           Api

main :: IO ()
main = generateElmModuleWith
  defElmOptions
  ["Api"]
  defElmImports
  "client/src"
  [DefineElm (Proxy :: Proxy Item), DefineElm (Proxy :: Proxy ItemId)]
  (Proxy :: Proxy Api)
