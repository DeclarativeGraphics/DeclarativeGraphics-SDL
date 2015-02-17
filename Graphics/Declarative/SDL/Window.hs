module Graphics.Declarative.SDL.Window where

import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.UI.SDL as SDL
import Graphics.Declarative.Cairo.Form
import Graphics.Declarative.Cairo.Shape
import Control.Applicative
import Control.Monad
import Data.Bits
import Foreign.Marshal.Alloc
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import System.Endian
import GHC.Word
import Loop

import Graphics.Declarative.SDL.Input

data WindowInfo = WindowInfo 
  { title :: String,
    width :: Int,
    height :: Int }

type Risky a = Either String a

withSDLWindow :: WindowInfo -> (SDL.Window -> SDL.Renderer -> IO ()) -> IO ()
withSDLWindow winInfo windowAction = do
    initializeSDL [SDL.initFlagVideo] >>= either throwSDLError return
    window <- createWindow (title winInfo) (fromIntegral $ width winInfo) (fromIntegral $ height winInfo) >>= either throwSDLError return
    renderer <- SDL.createRenderer window (-1) rflags
    windowAction window renderer
    SDL.destroyWindow window
    SDL.quit
  where
    rflags = (.|.) SDL.rendererFlagPresentVSync SDL.rendererFlagAccelerated

initializeSDL :: [Word32] -> IO (Risky ())
initializeSDL flags = do
    initSuccess <- SDL.init $ foldl (.|.) 0 flags
    return $ if initSuccess < 0 then Left "SDL could not initialize!" else Right ()

createWindow :: String -> CInt -> CInt -> IO (Risky SDL.Window)
createWindow windowTitle windowWidth windowHeight = withCAString windowTitle $ \title -> do
    window <- SDL.createWindow title SDL.windowPosUndefined SDL.windowPosUndefined windowWidth windowHeight SDL.windowFlagShown
    return $ if window == nullPtr then Left "Window could not be created!" else Right window

throwSDLError :: String -> IO a
throwSDLError message = do
    errorString <- SDL.getError >>= peekCString
    fail (message ++ " SDL_Error: " ++ errorString)

applyToPointer :: (Storable a) => (a -> b) -> Ptr a -> IO b
applyToPointer operation pointer = liftM operation $ peek pointer

sdlEventLoop :: Int -> state -> SDL.Window -> SDL.Renderer -> (Input -> state -> IO state)
sdlEventLoop runsPerSec initState win rend = scheduledLoop runsPerSec initState $ \currentState -> do


pollSDLEvent :: IO (Bool, Maybe Input)
pollSDLEvent = alloca $ \eventptr -> do
    status <- SDL.pollEvent eventptr
    if status == 1 then do
      event <- peek eventptr
      case event of
        (SDL.QuitEvent _ _) -> return (False, Nothing)
        e -> return (True, fromSDLEvent e)
    else (True, Nothing)


sdlEventLoop :: SDL.Renderer -> SDL.Window -> state -> IO (Bool, state)
sdlEventLoop renderer window state@(Widget _ oldInputAut _) =
  alloca $ \eventptr -> do
    status <- SDL.pollEvent eventptr
    if status == 1 then do
      event <- peek eventptr
      case event of
        (SDL.QuitEvent _ _) -> return (False, state)
        e -> case fromSDLEvent e of
          Just input -> renderState renderer window $ state { currentState = newState, inputAut = newInputAut } where (newInputAut, newState) = runA oldInputAut input
          Nothing    -> renderState renderer window state
    else renderState renderer window state
  where
    renderState renderer window (Widget out inputAut renderAut) =
      alloca $ \wptr      ->
      alloca $ \hptr      -> do
        SDL.getWindowSize window wptr hptr

        w <- fromIntegral <$> peek wptr
        h <- fromIntegral <$> peek hptr

        let (newAut, renderedGraphic) = runA renderAut out
        render renderer window $ unpackEnveloped $ renderedGraphic `atop` filled (1, 1, 1) (fromEnvelope (Envelope 0 0 w h))
        return (True, Widget out inputAut newAut)


-- Borrowed from http://hackage.haskell.org/package/helm-0.6.1/docs/src/FRP-Helm.html#render
renderForm :: SDL.Renderer -> SDL.Window -> Form -> IO ()
renderForm renderer window graphic =
  alloca $ \wptr      ->
  alloca $ \hptr      ->
  alloca $ \pixelsptr ->
  alloca $ \pitchptr  -> do
    SDL.getWindowSize window wptr hptr

    w <- fromIntegral <$> peek wptr
    h <- fromIntegral <$> peek hptr

    format <- SDL.masksToPixelFormatEnum 32 (fromBE32 0x0000ff00) (fromBE32 0x00ff0000) (fromBE32 0xff000000) (fromBE32 0x000000ff)
    texture <- SDL.createTexture renderer format SDL.textureAccessStreaming (fromIntegral w) (fromIntegral h)

    SDL.lockTexture texture nullPtr pixelsptr pitchptr

    pixels <- peek pixelsptr
    pitch <- fromIntegral <$> peek pitchptr

    res <- Cairo.withImageSurfaceForData (castPtr pixels) Cairo.FormatARGB32 w h pitch $ \surface ->
      Cairo.renderWith surface (drawForm graphic)

    SDL.unlockTexture texture

    SDL.setRenderDrawColor renderer 255 255 255 255
    SDL.renderClear renderer
    SDL.renderCopy renderer texture nullPtr nullPtr
    SDL.destroyTexture texture
    SDL.renderPresent renderer
