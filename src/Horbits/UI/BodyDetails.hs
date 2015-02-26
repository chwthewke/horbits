{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TypeSynonymInstances      #-}

module Horbits.UI.BodyDetails where

import           Control.Applicative
import           Control.Lens                         hiding ((*~))
import           Control.Monad                        hiding (ap, forM_, mapM_, sequence_)
import           Data.Foldable
import           Graphics.UI.Gtk                      as Gtk
import           Horbits.Body
import           Horbits.Orbit
import           Horbits.SolarSystem
import           Horbits.UI.ShowDim
import           Numeric.Units.Dimensional.TF.Prelude hiding (mapM_, sequence_)
import           Prelude                              hiding (mapM_, sequence_, (+), (/))

-- TODO layout (sizes, L/R align)
-- TODO VScroll

-- Detail definitions

data Detail s a = Detail (Maybe String) (Maybe a) (Fold s a)

instance Functor (Detail s) where
    fmap f (Detail t d p) = Detail t (f <$> d) (p . to f)

detailOf :: Fold s a -> Detail s a
detailOf = Detail Nothing Nothing

titled :: String -> Detail s a -> Detail s a
titled t (Detail _ d p) = Detail (Just t) d p

defaulting :: a -> Detail s a -> Detail s a
defaulting d (Detail t _ p) = Detail t (Just d) p


data DetailsSection = forall s. DetailsSection (Maybe String) (Fold Body s) [Detail s String]


-- Single Line
data BodyDetailLabel a = BodyDetailLabel { bodyDetailLabelView    :: HBox
                                         , bodyDetailLabelSetData :: a -> IO ()
                                         }

bodyDetailLabelNew :: Detail s String -> IO (BodyDetailLabel s)
bodyDetailLabelNew (Detail name def prop) = do
    hb <- hBoxNew True 5
    -- title
    forM_ name (\n -> labelNew (Just n) >>= containerAdd hb)
    -- label
    valueLabel <- labelNew Nothing
    _ <- containerAdd hb valueLabel
    -- label update
    let update mv = do
        forM_ mv $ labelSetMarkup valueLabel
        Gtk.set hb [ widgetVisible := has _Just mv ]
    _ <- update def
    return $ BodyDetailLabel hb (update . preview prop)


-- Section
data BodyDetailsSectionBox = BodyDetailsSectionBox { bodyDetailsSectionView :: VBox
                                                   , bodyDetailsSectionSetBody :: Body -> IO () }

titleLabel :: String -> IO Label
titleLabel t = do
    l <- labelNew Nothing
    labelSetMarkup l $ "<b>" ++ t ++ "</b>"
    return l

bodyDetailsSectionNew :: DetailsSection -> IO BodyDetailsSectionBox
bodyDetailsSectionNew (DetailsSection title prop details) = do
    vb <- vBoxNew True 5
    forM_ title $ containerAdd vb <=< titleLabel
    detailLabels <- mapM bodyDetailLabelNew details
    forM_ detailLabels $ containerAdd vb . bodyDetailLabelView
    return $ BodyDetailsSectionBox vb (setBody vb detailLabels)
  where
    setBody box labels b = do
        forM_ (b ^? prop) (forM_ labels . flip bodyDetailLabelSetData)
        Gtk.set box [ widgetVisible := has _Just (b ^? prop) ]

-- Whole details pane
data BodyDetailsPane = BodyDetailsPane { bodyDetailsPaneView    :: VBox
                                       , bodyDetailsPaneSetBody :: Body -> IO ()
                                       }

bodyDetailsSections :: [DetailsSection] -> IO BodyDetailsPane
bodyDetailsSections sections = do
    pane <- vBoxNew False 5
    detailsWidgets <- forM sections bodyDetailsSectionNew
    forM_ detailsWidgets (containerAdd pane . bodyDetailsSectionView)
    let setBody = forM_ detailsWidgets . flip bodyDetailsSectionSetBody
    return $ BodyDetailsPane pane setBody

bodyDetailsPaneNew :: IO BodyDetailsPane
bodyDetailsPaneNew = bodyDetailsSections detailsSections

-- Actual content
--


headerSection :: DetailsSection
headerSection = DetailsSection Nothing (to id)
    [ detailOf bodyName
    , detailOf bodyId <&> bodyPosition <&> showBodyPosition
    ]

orbitalSection :: DetailsSection
orbitalSection = DetailsSection (Just "Orbital Characteristics") (bodyId . bodyOrbit)
    [ detailOf semiMajorAxis & titled "Semi-major axis" <&> showOrbitalDistance
    , detailOf apoapsisHeight & titled "Apoapsis" <&> showOrbitalDistance
    , detailOf periapsisHeight & titled "Periapsis" <&> showOrbitalDistance
    , detailOf eccentricity & titled "Eccentricity" <&> showDegreeAngle
    , detailOf inclination & titled "Inclination" <&> showDegreeAngle
    , detailOf argumentOfPeriapsis & titled "Argument of periapsis" <&> showDegreeAngle
    , detailOf rightAscensionOfAscendingNode & titled "Longitude of asc. node" <&> showDegreeAngle
    , detailOf meanAnomalyAtEpoch & titled "Longitude of asc. node" <&> showRadianAngle
    , detailOf orbitalPeriod & titled "Orbital period" <&> showKerbalTime
    , detailOf orbitalVelocity & titled "Orbital Velocity" <&> showOrbitalVelocity
    ]

physicalSection :: DetailsSection
physicalSection = DetailsSection (Just "Physical Characteristics") (to id)
    [ detailOf bodyRadius & titled "Equatorial radius" <&> showPlanetaryDistance
    , detailOf bodySurfaceArea & titled "Surface area" <&> showQuantitySci
    , detailOf bodyMass & titled "Mass" <&> showQuantitySci
    , detailOf bodyGravitationalParam & titled "Gravitational parameter" <&> showQuantitySci
    , detailOf bodyDensity & titled "Density" <&> showQuantity
    , detailOf bodySurfaceGravity & titled "Surface gravity" <&> showQuantity
    , detailOf bodyEscapeVelocity & titled "Escape velocity" <&> showQuantity
    , detailOf bodySiderealRotationPeriod & titled "Sidereal rotation period" <&> showKerbalTime
    , detailOf bodySiderealRotationVelocity & titled "Sidereal rotation velocity" <&> showQuantity
    , detailOf bodySolarDay & titled "Solar Day" <&> showKerbalTime
    , detailOf bodySynchronousOrbitAltitude & titled "Synchronous orbit" <&> showOrbitalDistance
    , detailOf bodySphereOfInfluence & titled "Sphere of influence" <&> showOrbitalDistance
    ]

detailsSections :: [DetailsSection]
detailsSections = [headerSection, orbitalSection, physicalSection]

