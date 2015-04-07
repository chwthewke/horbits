module HorbitsCC.CameraControlExploration(basesAndCameraControls, ZoomModel(..), linearZoom, geometricZoom) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad             hiding (forM_)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Data.Binding.Simple
import           Data.Foldable
import           Data.IORef
import           Data.List.NonEmpty        (NonEmpty ((:|)))
import qualified Data.List.NonEmpty        as NE
import           Graphics.Rendering.OpenGL
import           Graphics.UI.Gtk           hiding (set)
import           Horbits.UI.Camera
import           Horbits.UI.GL.GLCamera
import           Horbits.UI.GL.GLSetup
import           Linear
import           Prelude                   hiding (foldr)

-- Cam tests

drawBasis :: (VertexComponent a, RealFloat a) => Vertex4 a -> IO ()
drawBasis (Vertex4 x y z w) =
    renderPrimitive Lines $ do
        color (Color3 1.0 0.0 0.0 :: Color3 GLfloat)
        vertex $ Vertex3 x y z
        vertex $ Vertex3 (x + w) y z
        color (Color3 0.0 1.0 0.0 :: Color3 GLfloat)
        vertex $ Vertex3 x y z
        vertex $ Vertex3 x (y + w) z
        color (Color3 0.0 0.0 1.0 :: Color3 GLfloat)
        vertex $ Vertex3 x y z
        vertex $ Vertex3 x y (z + w)

drawBases :: IO ()
drawBases = do
    drawBasis (Vertex4 0.0 0.0 0.0 1.0 :: Vertex4 GLfloat)
    drawBasis (Vertex4 2.0 0.0 0.0 0.7 :: Vertex4 GLfloat)
    drawBasis (Vertex4 (-2.0) 0.0 0.0 0.7 :: Vertex4 GLfloat)
    drawBasis (Vertex4 0.0 2.0 0.0 0.7 :: Vertex4 GLfloat)
    drawBasis (Vertex4 0.0 (-2.0) 0.0 0.7 :: Vertex4 GLfloat)
    drawBasis (Vertex4 0.0 0.0 2.0 0.7 :: Vertex4 GLfloat)
    drawBasis (Vertex4 0.0 0.0 (-2.0) 0.7 :: Vertex4 GLfloat)

-- TODO These may need to go, later
degrees :: (Floating a) => Iso' a a
degrees = iso (* (180 / pi)) (* (pi / 180))

orthoCameraColatitudeDeg :: Floating a => Lens' (OrthoCamera a) a
orthoCameraColatitudeDeg = orthoCameraColatitude . degrees

orthoCameraLongitudeDeg :: Floating a => Lens' (OrthoCamera a) a
orthoCameraLongitudeDeg = orthoCameraLongitude . degrees
--

basesAndCameraControls :: Window -> IO ()
basesAndCameraControls window = do
    cam <- orthoCameraNew 1 600 600
    box <- hBoxNew False 5
    ctrlBox <- vBoxNew True 5
    containerAdd box ctrlBox
    boundSpinButton cam (orthoCameraCenter . _x) "x" (-5) 5 1 (Just 0) >>= containerAdd ctrlBox
    boundSpinButton cam (orthoCameraCenter . _y) "y" (-5) 5 1 (Just 0) >>= containerAdd ctrlBox
    boundSpinButton cam (orthoCameraCenter . _z) "z" (-5) 5 1 (Just 0) >>= containerAdd ctrlBox
    boundSpinButton cam orthoCameraScale "d" 1 20 1 Nothing >>= containerAdd ctrlBox
    boundSpinButton cam orthoCameraColatitudeDeg "colat" 0 180 5 Nothing >>= containerAdd ctrlBox
    boundSpinButton cam orthoCameraLongitudeDeg "long" 0 360 5 Nothing >>= containerAdd ctrlBox
    resizeCb <- bindCameraToGL cam
    canvas <- setupGL 600 600 resizeCb drawBases
    setupMouseEvents canvas (linearZoom 1 1 20) cam
    containerAdd box canvas
    containerAdd window box

boundSpinButton :: (Variable v) => v t -> ASetter' t Double -> String -> Double -> Double -> Double -> Maybe Double -> IO HBox
boundSpinButton src prop lab min' max' step def = do
    box <- hBoxNew True 5
    sb <- spinButtonNewWithRange min' max' step
    forM_ def $ spinButtonSetValue sb
    _ <- onValueSpinned sb $ spinButtonGetValue sb >>= (modifyVar src . set prop)
    _ <- labelNew (Just lab) >>= containerAdd box
    containerAdd box sb
    return box

onMouseMoveLeft :: (Variable v, RealFloat a, Epsilon a) => v (OrthoCamera a) -> (Double, Double) -> IO ()
onMouseMoveLeft cam (dx, dy) =
    runStateVar cam $ do
        w <- use orthoCameraViewportWidth
        h <- use orthoCameraViewportHeight
        let v = V2 (2 * realToFrac dx / fromIntegral w) (-2 * realToFrac dy / fromIntegral h)
        modify (addTranslation v)

onMouseMoveRight :: (Variable v, RealFloat a, Epsilon a) => v (OrthoCamera a) -> (Double, Double) -> IO ()
onMouseMoveRight cam (dx, dy) =
    runStateVar cam $ do
        w <- use orthoCameraViewportWidth
        h <- use orthoCameraViewportHeight
        modify . addColatitude . negate $ (pi * realToFrac dy / fromIntegral w)
        modify . addLongitude $ (pi * realToFrac dx / fromIntegral h)

onMouseScroll :: (Variable v, Num a) => v (OrthoCamera a) -> ZoomModel a -> ScrollDirection -> IO ()
onMouseScroll cam zm dir =
    runStateVar cam $ do
        let z = if dir == ScrollUp then zoomIn else zoomOut
        orthoCameraScale %= z zm

data MButton = LButton | RButton deriving (Show, Eq, Ord)
mButton :: MouseButton -> Maybe MButton
mButton LeftButton = Just LButton
mButton RightButton = Just RButton
mButton _ = Nothing


data MState = MState [MButton] (Double, Double)

runStateVar :: (MonadIO m, Variable v) => v s -> StateT s m a -> m a
runStateVar v st = do
    i <- liftIO $ readVar v
    (r, o) <- runStateT st i
    _ <- liftIO $ writeVar v o
    return r

onButtonEvent :: (Variable v) => (MButton -> [MButton] -> [MButton]) -> v MState -> EventM EButton ()
onButtonEvent f st = do
    button <- eventButton
    coords <- eventCoordinates
    liftIO $ forM_ (mButton button) (modifyVar st . newState coords)
  where
    newState c b (MState bs _) = MState (f b bs) c


onMouseMove :: (Variable v1, Variable v2, RealFloat a, Epsilon a) =>
               v1 (OrthoCamera a) -> v2 MState -> EventM t (Double, Double) -> EventM t ()
onMouseMove cam st evCoords = do
    (coords @ (cx, cy)) <- evCoords
    MState buttons (sx, sy) <- liftIO $ readVar st
    liftIO $ writeVar st (MState buttons coords)
    case buttons of
        LButton : _ -> liftIO $ onMouseMoveLeft cam  (cx - sx, cy - sy)
        RButton : _ -> liftIO $ onMouseMoveRight cam  (cx - sx, cy - sy)
        _ -> return ()

data ZoomModel a = ZoomModel { zoomIn  :: a -> a
                             , zoomOut :: a -> a }

linearZoom :: (Num a, Ord a) => a -> a -> a -> ZoomModel a
linearZoom step minScale maxScale = listZoom scales
  where
    scales = NE.unfold f minScale
    f s = (s, (+ step) <$> mfilter (< maxScale) (Just s))

geometricZoom :: (Num a, Ord a) => a -> a -> a -> ZoomModel a
geometricZoom step minScale maxScale = listZoom scales
  where
    scales = NE.unfold f minScale
    f s = (s, (* step) <$> mfilter (< maxScale) (Just s))

listZoom :: (Ord a) => NonEmpty a -> ZoomModel a
listZoom zooms = ZoomModel (\z -> NE.last $ NE.head zooms :| NE.takeWhile (< z) zooms)
                           (\z -> NE.head $ foldr NE.cons (NE.last zooms :| []) (NE.dropWhile (<= z) zooms))

setupMouseEvents :: (WidgetClass w, Variable v, RealFloat a, Epsilon a) => w -> ZoomModel a -> v (OrthoCamera a) -> IO ()
setupMouseEvents w zm cam = do
    st <- newVar (MState [] (0.0, 0.0)) :: IO (IORef MState)
    widgetAddEvents w
        [PointerMotionHintMask,
        Button1MotionMask, Button3MotionMask,
        ScrollMask,
        ButtonPressMask, ButtonReleaseMask]
        -- TODO the last three should be set by the listeners on press/release/scroll
    void $ on w motionNotifyEvent $ tryEvent $ do
        onMouseMove cam st eventCoordinates
        eventRequestMotions
    void $ on w buttonPressEvent $ tryEvent $
        onButtonEvent (\b bs -> b : filter (/= b) bs) st
    void $ on w buttonReleaseEvent $ tryEvent $
        onButtonEvent (\b bs -> filter (/= b) bs) st
    void $ on w scrollEvent $ tryEvent $ do
        d <- eventScrollDirection
        liftIO $ onMouseScroll cam zm d
