
module Main where

import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL

import Control.Monad

import Data.ByteString.Unsafe
import qualified Data.ByteString as BS
import Codec.Image.PNG
import System.IO
import Data.Array.Storable
import System
import System.CPUTime

main :: IO ()
main = setupOpenGL

(winWidth, winHeight) = (640,480)

setupOpenGL :: IO ()
setupOpenGL = do
  (_, [fn]) <- getArgsAndInitialize
  initialWindowSize $= Size winWidth winHeight
  initialDisplayMode $= [RGBAMode, WithDepthBuffer]
  win <- createWindow "PNGLoader test"
  displayCallback $= displayFn
  setupTexture fn
  mainLoop

displayFn :: IO ()
displayFn = do
  viewport $= (Position 0 0, Size winWidth winHeight)
  matrixMode $= Projection
  loadIdentity
  ortho2D 0 (fromIntegral winWidth) 0 (fromIntegral winHeight)
  scale 1.0 (-1.0) (1.0::GLfloat)
  translate (Vector3 0.0 (fromIntegral (-winHeight)) (0.0::GLfloat))
  matrixMode $= Modelview 0
  loadIdentity

  clearColor $= Color4 0 0 0 0
  clear [ColorBuffer, DepthBuffer]
  texture Texture2D $= Enabled
  textureBinding Texture2D $= Just (TextureObject 0)
  renderTexture 0 0 640 480
  swapBuffers

renderTexture :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
renderTexture x y w h = do
  renderPrimitive Quads $ do texCoord (TexCoord2 0 (0::GLfloat))
                             vertex (Vertex2 x y)
                             texCoord (TexCoord2 1 (0::GLfloat))
                             vertex (Vertex2 (x+w) y)
                             texCoord (TexCoord2 1 (1::GLfloat))
                             vertex (Vertex2 (x+w) (y+h))
                             texCoord (TexCoord2 0 (1::GLfloat))
                             vertex (Vertex2 x (y+h))

setupTexture :: FilePath -> IO ()
setupTexture fn = do
  startTime <- getCPUTime
  x <- loadPNGFile fn
  print x
  stopTime <- getCPUTime
  putStrLn $ "Loading took " ++ show ((stopTime - startTime) `div` 1000000000) ++ " milliseconds"
  case x of
    Left s   -> (putStrLn $ "ERROR: " ++ s) >> exitWith (ExitFailure (-1))
    Right img -> do 
           texture Texture2D $= Enabled
           textureBinding Texture2D $= Just (TextureObject 0)
           let (w,h) = dimensions img
               (a,b) = case hasAlphaChannel img of
                         True -> (RGBA8, RGBA)
                         False -> (RGB8, RGB)
           withStorableArray (imageData img) $ \ptr ->
               build2DMipmaps Texture2D a (fromIntegral w) (fromIntegral h) (PixelData b UnsignedByte ptr)
