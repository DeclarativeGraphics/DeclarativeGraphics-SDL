{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Declarative.SDL.Glue where

import qualified Graphics.Rendering.Cairo as Cairo
import qualified SDL
import SDL (($=), get)
import Data.Text
import Linear
import Data.Bits
import Control.Applicative
import Foreign.Marshal.Alloc
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import System.Endian
import Control.Exception


runSDL :: IO () -> IO ()
runSDL = bracket_ (SDL.initialize [SDL.InitVideo]) SDL.quit

withWindow :: String -> SDL.WindowConfig -> (SDL.Window -> IO ()) -> IO ()
withWindow title config =
    bracket (SDL.createWindow (pack title) config) SDL.destroyWindow

withRenderer :: SDL.RendererConfig -> SDL.Window -> (SDL.Renderer -> IO ()) -> IO ()
withRenderer rendererConf window =
    bracket (SDL.createRenderer window (-1) rendererConf) SDL.destroyRenderer

withTexture :: SDL.Renderer -> V2 CInt -> (SDL.Texture -> IO ()) -> IO ()
withTexture rend size =
    bracket (SDL.createTexture rend SDL.ARGB8888 SDL.TextureAccessStreaming size) SDL.destroyTexture

withPixelsFromTexture :: SDL.Texture -> (Int -> Ptr () -> IO ()) -> IO ()
withPixelsFromTexture texture renderer =
    bracket
       (SDL.lockTexture texture Nothing)
       (\_ -> SDL.unlockTexture texture)
       (\(pixels, pitch) -> renderer (fromIntegral pitch) pixels)

-- RENDERING

renderCairoViaTexture :: Cairo.Render () -> SDL.Window -> SDL.Renderer -> IO ()
renderCairoViaTexture cairoRend win rend = do
  size@(V2 w h) <- get $ SDL.windowSize win
  withTexture rend size $ \ texture -> do
      withPixelsFromTexture texture $ cairoRenderToPixels cairoRend (fromIntegral w) (fromIntegral h)
      sdlRenderTexture rend texture

sdlRenderTexture :: SDL.Renderer -> SDL.Texture -> IO ()
sdlRenderTexture renderer texture = do
  SDL.rendererDrawColor renderer $= V4 255 255 255 255
  SDL.clear renderer
  SDL.copy renderer texture Nothing Nothing
  SDL.present renderer
  return ()

cairoRenderToPixels :: Cairo.Render () -> Int -> Int -> Int -> Ptr () -> IO ()
cairoRenderToPixels graphic w h pitch pixels =
  Cairo.withImageSurfaceForData (castPtr pixels) Cairo.FormatARGB32 w h pitch $ \ surface ->
    Cairo.renderWith surface graphic

-- destroys texture's content!!!
resizeTexture :: SDL.Renderer -> SDL.Texture -> IO SDL.Texture
resizeTexture rend texture = do
  (SDL.TextureInfo format access width height) <- SDL.queryTexture texture
  SDL.destroyTexture texture
  SDL.createTexture rend format access $ V2 (fromIntegral width) (fromIntegral height)

cairoClear :: Cairo.Render ()
cairoClear = setSourceColor (1, 1, 1) >> Cairo.paint

type RGB = (Double, Double, Double)

setSourceColor :: RGB -> Cairo.Render ()
setSourceColor (r,g,b) = Cairo.setSourceRGB r g b
