{-# OPTIONS_GHC -fplugin=Lib #-}

module Test where

import Debug

main :: String
main = debug (Just id)

