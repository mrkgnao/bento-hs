module Main where

import           Data.Aeson (toJSON)
import           Lib        (defaultSetup)

main :: IO ()
main = print $ toJSON defaultSetup
