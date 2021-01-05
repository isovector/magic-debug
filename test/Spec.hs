{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fplugin=Lib  #-}

import Debug

main :: IO ()
main = do
  putStrLn $ debug id
  putStrLn $ debug @(Int -> Int) id
  putStrLn $ debug True
  putStrLn $ debug "hello world"
  putStrLn $ debug $ Just not

