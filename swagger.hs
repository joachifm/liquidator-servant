module Main (main) where

import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as LB
import Api (swaggerDoc)

main :: IO ()
main = LB.putStrLn $ encodePretty swaggerDoc
