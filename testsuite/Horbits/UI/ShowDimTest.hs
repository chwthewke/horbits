{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types                #-}

module Horbits.UI.ShowDimTest where

import           Control.Rematch
import           Numeric.Units.Dimensional.TF.Prelude
import           Prelude                              hiding ((*), (/))
import           Test.Framework

import           Horbits.Rematch
import           Horbits.UI.ShowDim

data TestCase = forall d. ShowDim d => TestCase (Quantity d Double) Double String

instance Show TestCase where
    show (TestCase _ x u) = show x ++ ", " ++ show u

testCases :: [TestCase]
testCases = [ TestCase (1 *~ meter) 1 " m"
            , TestCase (2 *~ (one / second)) 2 " /s"
            , TestCase (3.14159265359 *~ radian) 3.14159265359 ""
            , TestCase (10 *~ square meter) 10 " m<sup>2</sup>"
            , TestCase (3 *~ (mole / cubic meter)) 3 " mol/m<sup>3</sup>"
            , TestCase (6.67384e-11 *~ (cubic meter / (kilo gram * second * second)))
                6.67384e-11 " m<sup>3</sup>/kg s<sup>2</sup>" ]

checkTestCase :: TestCase -> Property
checkTestCase (TestCase q x u) = showUnit q `shouldBe` equalTo (x, u)

prop_showUnitTestCases :: Property
prop_showUnitTestCases = forAll (elements testCases) checkTestCase
