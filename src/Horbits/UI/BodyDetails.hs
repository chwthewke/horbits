{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TypeFamilies              #-}

module Horbits.UI.BodyDetails where

import           Control.Lens
import           Control.Monad
import           Data.Monoid
import           Graphics.UI.Gtk
import           Horbits.Body
import           Numeric.Units.Dimensional.TF.Prelude

data BodyDetailsPane = BodyDetailsPane { bodyDetailsPaneView    :: VBox
                                       , bodyDetailsPaneSetBody :: Body -> IO ()
                                       }

bodyDetailsPaneNew :: IO BodyDetailsPane
bodyDetailsPaneNew = do
    pane <- vBoxNew True 5
    detailLabels <- forM details bodyDetailLabelNew
    forM_ detailLabels (containerAdd pane . bodyDetailLabelView)
    return $ BodyDetailsPane pane (sequence_ . mapM bodyDetailLabelSetBody detailLabels)

data Detail = forall a. (Show a) => Detail String a (Getting a Body a)

details :: [Detail]
details = [ Detail "Name" mempty bodyName
          , Detail "Gravitational parameter" _0 (bodyGravitationalParam . _Wrapped')
          , Detail "Radius" _0 (bodyRadius . _Wrapped')
          ]

data BodyDetailLabel = BodyDetailLabel { bodyDetailLabelView    :: HBox
                                       , bodyDetailLabelSetBody :: Body -> IO ()
                                       }

bodyDetailLabelNew :: Detail -> IO BodyDetailLabel
bodyDetailLabelNew (Detail name def prop) = do
    hb <- hBoxNew True 5
    descLabel <- labelNew $ Just name
    valueLabel <- labelNew . Just $ show def
    _ <- containerAdd hb descLabel
    _ <- containerAdd hb valueLabel
    return $ BodyDetailLabel hb (labelSetText valueLabel . show . view prop)



