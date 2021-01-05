{-# OPTIONS_GHC -fplugin=Lib #-}

module Test where

import Debug

test :: (a -> b) -> String
test f = debug f

