{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where


import           Test.Framework

import  {-@ HTF_TESTS @-} Horbits.KerbalDateTimeTest
import  {-@ HTF_TESTS @-} Horbits.OrbitClassicalTest
import  {-@ HTF_TESTS @-} Horbits.OrbitGen
import  {-@ HTF_TESTS @-} Horbits.UI.ShowDimTest

main :: IO()
main = htfMain htf_importedTests

