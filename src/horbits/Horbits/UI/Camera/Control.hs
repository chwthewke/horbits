{-# LANGUAGE FlexibleContexts #-}

module Horbits.UI.Camera.Control (setupMouseControl) where

import           Control.Lens
import           Control.Monad.Trans.State
import           Data.IORef
import           Graphics.UI.Gtk
import           Linear

import           Horbits.Data.Binding
import           Horbits.UI.Camera.Internal

-- Ongoing mouse state
data MState = MState [MouseButton] (Double, Double)

-- Camera updates

mousePan :: (Monad m, RealFloat a, Epsilon a) => (Double, Double) -> StateT (OrthoCamera a) m ()
mousePan (dx, dy) = do
    w <- use orthoCameraViewportWidth
    h <- use orthoCameraViewportHeight
    let v = V2 (2 * realToFrac dx / fromIntegral w) (2 * realToFrac dy / fromIntegral h)
    modify (addTranslation v)

mouseRotate :: (Monad m, RealFloat a, Epsilon a) => (Double, Double) -> StateT (OrthoCamera a) m ()
mouseRotate (dx, dy) = do
    w <- use orthoCameraViewportWidth
    h <- use orthoCameraViewportHeight
    modify . addColatitude $ pi * realToFrac dy / fromIntegral w
    modify . addLongitude $ pi * realToFrac dx / fromIntegral h

mouseScroll :: (Monad m, Num a, Ord a) => ScrollDirection -> StateT (OrthoCamera a) m ()
mouseScroll dir = do
    let z = if dir == ScrollUp then zoomIn else zoomOut
    modify z

-- Mouse event processing

-- TODO map MState with lens?
onButtonEvent :: (HasUpdate v MState MState)
              => (MouseButton -> [MouseButton] -> [MouseButton]) -> v -> EventM EButton ()
onButtonEvent f st = do
    button <- eventButton
    coords <- eventCoordinates
    st $~ newState coords button -- TODO ??? zoom or sth
  where
    newState c b (MState bs _) = MState (f b bs) c


onMouseMove :: (HasGetter vs MState, HasSetter vs MState, 
                HasGetter vc (OrthoCamera a), HasSetter vc (OrthoCamera a),
                RealFloat a, Epsilon a) =>
               vc -> vs -> EventM t (Double, Double) -> EventM t ()
onMouseMove cam st evCoords = do
    (coords @ (cx, cy)) <- evCoords
    MState buttons (sx, sy) <- readVar st
    st $= MState buttons coords -- TODO MState manipulation is weak, see above, also <<%= (!)
    evalStateVar cam $ case buttons of
                        LeftButton : _ -> mousePan (cx - sx, sy - cy)
                        RightButton : _ -> mouseRotate (cx - sx, sy - cy)
                        _ -> return ()

setupMouseControl :: (HasGetter v (OrthoCamera a), HasSetter v (OrthoCamera a), 
                      WidgetClass w, RealFloat a, Epsilon a) 
                  => w -> v -> IO [ConnectId w]
setupMouseControl w cam = do
    st <- newVar (MState [] (0.0, 0.0)) :: IO (IORef MState)
    widgetAddEvents w [PointerMotionHintMask, Button1MotionMask, Button3MotionMask]
    sequence [
        on w motionNotifyEvent $ tryEvent $ do
            onMouseMove cam st eventCoordinates
            eventRequestMotions,
        on w buttonPressEvent $ tryEvent $
            onButtonEvent (\b bs -> b : filter (/= b) bs) st,
        on w buttonReleaseEvent $ tryEvent $
            onButtonEvent (\b bs -> filter (/= b) bs) st,
        on w scrollEvent $ tryEvent $ do
            d <- eventScrollDirection
            evalStateVar cam $ mouseScroll d
        ]

