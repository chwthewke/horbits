{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where


import           Test.Framework

import  {-@ HTF_TESTS @-} Horbits.OrbitClassicalTest
import  {-@ HTF_TESTS @-} Horbits.OrbitGen

main :: IO()
main = htfMain htf_importedTests

