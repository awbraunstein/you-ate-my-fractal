module Main where

import Draw
import QTree
import Fractals
import System.Environment

main :: IO ()
main = do
       args <- getArgs
       case args of
         [] -> putStrLn $ unlines [ "Usage: you-ate-my-fractal <fractal> <depth> <x1 y1 x2 y2>",
                                    "fractal: mandel",
                                    "         newton",
                                    "         julia",
                                    "         test",
                                    "Depth is an optional parameter",
                                    "Depth is recursion depth for generating the colors",
                                    "If Depth is ommited, the default of 100 is used",
                                    "The range is an optional parameters.",
                                    "(x1, y1) are top left",
                                    "(x2, y2) are bottom right",
                                    "if it is ommitted, (-2, 2), (2, -2) is used"
                                  ]

         otherwise ->
           draw ((case head args of
                   "newton"  -> newton
                   "mandel"  -> mandel
                   "julia"   -> julia
                   "test"    -> test
                   otherwise -> newton)
                  (case map read (tail args) of
                     [d, _, _, _, _] -> d
                     [d] -> d
                     otherwise -> defaultMaxIteration))
                  (case map read (tail args) of
                     [x1, y1, x2, y2] -> (Q x1 y1 x2 y2)
                     [_, x1, y1, x2, y2] -> (Q x1 y1 x2 y2)
                     otherwise -> defaultRange
                  )