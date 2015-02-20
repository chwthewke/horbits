{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TypeFamilies              #-}

module Horbits.UI.BodyDetails where

import           Control.Lens
import           Control.Monad                        hiding (ap, forM_, mapM_, sequence_)
import           Data.Foldable
import           Graphics.UI.Gtk as Gtk
import           Horbits.Body
import           Horbits.Orbit
import           Horbits.SolarSystem
import           Numeric.Units.Dimensional.TF.Prelude hiding (mapM_, sequence_)
import           Prelude                              hiding (mapM_, sequence_, (+))

-- TODO layout and rendering (Show (Dimensional v d) not enough)

-- Detail definitions
data Detail s = forall a. (Show a) => Detail (Maybe String) a (Getting a s a)
data DetailsSection = forall s. DetailsSection (Maybe String) (Fold Body s) [Detail s]

-- Single Line
data BodyDetailLabel a = BodyDetailLabel { bodyDetailLabelView    :: HBox
                                         , bodyDetailLabelSetData :: a -> IO ()
                                         }

bodyDetailLabelNew :: Detail a -> IO (BodyDetailLabel a)
bodyDetailLabelNew (Detail name def prop) = do
    hb <- hBoxNew True 5
    forM_ name (\n -> labelNew (Just n) >>= containerAdd hb)
    valueLabel <- labelNew . Just $ show def
    _ <- containerAdd hb valueLabel
    return $ BodyDetailLabel hb (labelSetText valueLabel . show . view prop)


-- Section
data BodyDetailsSectionWidgets = BodyDetailsSectionWidgets { bodyDetailsSectionWidgets :: [Widget]
                                                           , bodyDetailsSectionSetBody :: Body -> IO () }

titleLabel :: Maybe String -> IO [Widget]
titleLabel ms = forM (ms ^.. _Just) (\s -> fmap toWidget $ do
    l <- labelNew Nothing
    labelSetMarkup l $ "<b>" ++ s ++ "</b>"
    return l)

bodyDetailsSectionLabels :: DetailsSection -> IO BodyDetailsSectionWidgets
bodyDetailsSectionLabels (DetailsSection title prop details) = do
    titleWidget <- titleLabel title
    detailLabels <- mapM bodyDetailLabelNew details
    let widgets = titleWidget ++ fmap (toWidget . bodyDetailLabelView) detailLabels
    let setBody b = let propValue = b ^? prop in do
        forM_ propValue (forM_ detailLabels . flip bodyDetailLabelSetData)
        forM_ widgets $ flip Gtk.set [ widgetVisible := has _Just propValue ]
    return $ BodyDetailsSectionWidgets widgets setBody

-- Whole details pane
data BodyDetailsPane = BodyDetailsPane { bodyDetailsPaneView    :: VBox
                                       , bodyDetailsPaneSetBody :: Body -> IO ()
                                       }

bodyDetailsSections :: [DetailsSection] -> IO BodyDetailsPane
bodyDetailsSections sections = do
    pane <- vBoxNew True 5
    detailsWidgets <- forM sections bodyDetailsSectionLabels
    forM_ (detailsWidgets >>= bodyDetailsSectionWidgets) (containerAdd pane)
    let setBody = forM_ detailsWidgets . flip bodyDetailsSectionSetBody 
    return $ BodyDetailsPane pane setBody

bodyDetailsPaneNew :: IO BodyDetailsPane
bodyDetailsPaneNew = bodyDetailsSections detailsSections

-- Actual content

headerSection :: DetailsSection
headerSection = DetailsSection Nothing (to id)
    [ Detail Nothing "" bodyName
    , Detail Nothing "" (bodyId . to (show . bodyPosition))
    ]


orbitalSection :: DetailsSection
orbitalSection = DetailsSection (Just "Orbital Characteristics") (bodyId . bodyOrbit)
    [ Detail (Just "Semi-major axis") _0 semiMajorAxis
    , Detail (Just "Apoapsis") _0 apoapsisHeight
    , Detail (Just "Periapsis") _0 periapsisHeight
    , Detail (Just "Orbital Eccentricity") _0 eccentricity
    , Detail (Just "Orbital Inclination") _0 inclination
    , Detail (Just "Argument of periapsis") _0 argumentOfPeriapsis
    , Detail (Just "Longitude of asc. node") _0 rightAscensionOfAscendingNode
    , Detail (Just "Mean anomaly at epoch") _0 meanAnomalyAtEpoch
    , Detail (Just "Sidereal orbital period") _0 orbitalPeriod
    , Detail (Just "Orbital velocity") (OrbitalVelocity _0 _0) orbitalVelocity
    ]

physicalSection :: DetailsSection
physicalSection = DetailsSection (Just "Physical Characteristics") (to id)
    [ Detail (Just "Equatorial radius") _0 bodyRadius
    , Detail (Just "Surface area") _0 bodySurfaceArea
    , Detail (Just "Mass") _0 bodyMass
    , Detail (Just "Gravitational parameter") _0 bodyGravitationalParam
    , Detail (Just "Density") _0 bodyDensity
    , Detail (Just "Surface gravity") _0 bodySurfaceGravity
    , Detail (Just "Escape velocity") _0 bodyEscapeVelocity
    , Detail (Just "Sidereal rotation period") _0 bodySiderealRotationPeriod
    , Detail (Just "Sidereal rotation velocity") _0 bodySiderealRotationVelocity
    , Detail (Just "Solar day") Nothing . pre $ bodySolarDay
    , Detail (Just "Synchronous orbit") _0 bodySynchronousOrbitAltitude
    , Detail (Just "Sphere of influence") Nothing . pre $ bodySphereOfInfluence
    ] 

detailsSections :: [DetailsSection]
detailsSections = [headerSection, orbitalSection, physicalSection]

