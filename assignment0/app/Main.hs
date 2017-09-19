module Main where

import Curves

main :: IO ()
main = toFile 
    (hilbert $ hilbert $ hilbert $ hilbert $ curve (point (0,0)) [])
    "hilbert.svg"