module Horbits.UI.Camera.Control (setupMouseControl) where

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Data.IORef
import           Data.Variable
import           Graphics.UI.Gtk
import           Horbits.Data.Variable.State
import           Horbits.UI.Camera
import           Linear

-- Ongoing mouse state
data MState = MState [MouseButton] (Double, Double)

-- Camera updates

mousePan :: (Monad m, RealFloat a, Epsilon a) => (Double, Double) -> StateT (OrthoCamera a) m ()
mousePan (dx, dy) = do
    w <- use orthoCameraViewportWidth
    h <- use orthoCameraViewportHeight
    let v = V2 (2 * realToFrac dx / fromIntegral w) (-2 * realToFrac dy / fromIntegral h)
    modify (addTranslation v)

mouseRotate :: (Monad m, RealFloat a, Epsilon a) => (Double, Double) -> StateT (OrthoCamera a) m ()
mouseRotate (dx, dy) = do
    w <- use orthoCameraViewportWidth
    h <- use orthoCameraViewportHeight
    modify . addColatitude . negate $ (pi * realToFrac dy / fromIntegral w)
    modify . addLongitude $ (pi * realToFrac dx / fromIntegral h)

mouseScroll :: (Monad m, Num a, Ord a) => ScrollDirection -> StateT (OrthoCamera a) m ()
mouseScroll dir = do
    let z = if dir == ScrollUp then zoomIn else zoomOut
    modify z

-- Mouse event processing

onButtonEvent :: (Variable v) => (MouseButton -> [MouseButton] -> [MouseButton]) -> v MState -> EventM EButton ()
onButtonEvent f st = do
    button <- eventButton
    coords <- eventCoordinates
    liftIO $ modifyVar st $ newState coords button -- TODO ??? zoom or sth
  where
    newState c b (MState bs _) = MState (f b bs) c


onMouseMove :: (Variable v1, Variable v2, RealFloat a, Epsilon a) =>
               v1 (OrthoCamera a) -> v2 MState -> EventM t (Double, Double) -> EventM t ()
onMouseMove cam st evCoords = do
    (coords @ (cx, cy)) <- evCoords
    MState buttons (sx, sy) <- liftIO $ readVar st
    liftIO $ writeVar st (MState buttons coords) -- TODO MState manipulation is weak, see above, also <<%= (!)
    runStateVar cam $ case buttons of
                        LeftButton : _ -> mousePan (cx - sx, cy - sy)
                        RightButton : _ -> mouseRotate (cx - sx, cy - sy)
                        _ -> return ()

setupMouseControl :: (WidgetClass w, Variable v, RealFloat a, Epsilon a) => w -> v (OrthoCamera a) -> IO [ConnectId w]
setupMouseControl w cam = do
    st <- newVar (MState [] (0.0, 0.0)) :: IO (IORef MState)
    widgetAddEvents w
        [PointerMotionHintMask,
        Button1MotionMask, Button3MotionMask,
        ScrollMask,
        ButtonPressMask, ButtonReleaseMask]
        -- TODO the last three should be set by the listeners on press/release/scroll
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
            runStateVar cam $ mouseScroll d
        ]

