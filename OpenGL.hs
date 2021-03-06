{-# LANGUAGE GADTs, OverlappingInstances, UndecidableInstances, NoMonomorphismRestriction, CPP #-}
{-# OPTIONS -fglasgow-exts #-}

module OpenGL where 

import Data.IORef
--import qualified Allegro.Shape as S
import Control.Concurrent.STM
import System.Mem
import System.Time
import System.IO.Unsafe
--import Signal.TSignal
import Control.Concurrent
import Control.Monad
import Data.Typeable
import Data.Maybe
import Data.List (partition)
import Control.Monad.Trans
import Control.Monad.State.Strict
import EvalM 
--import SrcSinks
import Numbers
import System.Environment
#ifndef NOGL
import Graphics.Rendering.OpenGL hiding (Sink, get)
import Graphics.UI.GLFW -- hiding (Sink, get)
#endif



--import Allegro.Vector
--import qualified Data.Map as M


#ifndef NOGL
globalTVar = unsafePerformIO . newTVarIO

mouseXTVar :: TVar Int
{-# NOINLINE mouseXTVar #-}
mouseXTVar = globalTVar 0

mouseYTVar :: TVar Int
{-# NOINLINE mouseYTVar #-}
mouseYTVar = globalTVar 0
{-
mouseX' :: Source Int
mouseX' = SourceAction (readTV mouseXTVar)
mouseX :: Signal Double
mouseX = ((/300) . realToFrac) `fmap` Src mouseX'

mouseY' :: Source Int
mouseY' = SourceAction ((id) `fmap` readTV mouseYTVar)
mouseY :: Signal Double
mouseY = ((/300) . realToFrac) `fmap` Src mouseY'




leftMouseDownTVar :: TVar [Double]
{-# NOINLINE leftMouseDownTVar #-}
leftMouseDownTVar = unsafePerformIO $ newTVarIO ([])
leftMouseDown' :: EventSource ()
leftMouseDown' = EvtSource $ \t1 t2 -> fmap (map (\t->(t,()))) $ takeOutTV (\t-> t>t1 && t< t2) leftMouseDownTVar
leftMouseDown = EvtSrc leftMouseDown'

rightMouseDownTVar :: TVar [Double]
{-# NOINLINE rightMouseDownTVar #-}
rightMouseDownTVar = unsafePerformIO $ newTVarIO ([]::[Double])
rightMouseDown' :: EventSource ()
rightMouseDown' = EvtSource $ \t1 t2 -> fmap (map (\t->(t,()))) $ takeOutTV (\t-> t>t1 && t< t2) rightMouseDownTVar
rightMouseDown = EvtSrc rightMouseDown'


-}


{-globalTimeStartTVar:: TVar ClockTime
{-# NOINLINE globalTimeStartTVar #-}
globalTimeStartTVar = unsafePerformIO $ newTVarIO (undefined)

resetGlobalClock = getClockTime >>= writeTV globalTimeStartTVar

globalSecsNow :: IO Double
globalSecsNow = do tnow <- getClockTime
                   tstart <- readTV globalTimeStartTVar
                   return $ diffInS tnow tstart                  
    where diffInS (TOD t1s t1ps) (TOD t2s t2ps) = (fromInteger $ (t1s-t2s)*1000*1000 + ((t1ps-t2ps) `div` (1000*1000))) / 1000000
                   -}
readTV = atomically . readTVar
writeTV vr vl = atomically $ writeTVar vr vl
consTV vr vl = atomically $ do vls <- readTVar vr
                               writeTVar vr (vl:vls)
takeOutTV p tv = atomically $ do vls <- readTVar tv
                                 let (go,stay) = partition p vls
                                 writeTVar tv (stay)
                                 return go

setTexture fnm = do
  putStrLn $ "loading texture from "++fnm
  texture Texture2D $= Enabled
  [textureName] <- genObjectNames  1
  textureBinding Texture2D $= Just textureName
  textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
--  loadTexture2D "matheson.tga" []  
  loadTexture2D fnm []
  return ()

initGlScreen full dispFunMVar runningMVar moreStuff = do
  initialize
  args <- getArgs
  when ("-aa" `elem` args) $ do
    openWindowHint $= (FSAASamples, 1)
  let scrMode = if ("-full" `elem` args || full) then FullScreen else Window
  openWindow (Size 640 480) [
 --                 DisplayRGBBits 8 8 8,
                  DisplayAlphaBits 8
 --                 DisplayDepthBits 24,
 --                 DisplayStencilBits 0
                 ] scrMode
  windowTitle $= "Bugpan screen"
  swapInterval $= 1
  clearColor $= Color4 0 0.23 0 0
  clear [ColorBuffer]
  --ortho2D (-640) ( 640) (-480) ( 480)
  --shadeModel $= Smooth
  when ("-aa" `elem` args) $ do
    lineSmooth $= Enabled
    polygonSmooth $= Enabled
    hint PolygonSmooth $= Nicest
    hint LineSmooth $= Nicest
    hint PointSmooth $= Nicest
    --blend $= Enabled 
    --cullFace $= Just Back
    blendFunc $= (SrcAlphaSaturate, One)
  when ("-ms" `elem` args) $ do
    multisample $= Enabled
{-  texture Texture2D $= Enabled
  [textureName] <- genObjectNames  1
  textureBinding Texture2D $= Just textureName
  textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
  loadTexture2D "matheson.tga" []  -}
  moreStuff
  swapBuffers
  waitLoop dispFunMVar runningMVar

waitLoop dispFunMVar runMV = do
  dispFun <- readMVar dispFunMVar
  runGlSignals dispFun runMV
  clearColor $= Color4 0 0.23 0 0
  clear [ColorBuffer]
  swapBuffers

  waitLoop dispFunMVar runMV

runGlSignals dispPull runningMVar = do
  --wait until running
  readMVar runningMVar
  untilMVEmpty runningMVar $ display dispPull 

untilMVEmpty mv ma = do empty <- isEmptyMVar mv
                        if empty
                           then return ()
                           else ma >> untilMVEmpty mv ma
  

{-reshape s@(Size w h) = do 
  viewport $= (Position 0 0, s)
  postRedisplay Nothing

idle = do
    postRedisplay Nothing
-}
--keyboardMouse (MouseButton LeftButton) (Down) _ _ = globalSecsNow >>= consTV leftMouseDownTVar
--keyboardMouse (MouseButton RightButton) (Down) _ _ = globalSecsNow >>= consTV rightMouseDownTVar

display dispPull = do
 clear [ColorBuffer]
 matrixMode $= Projection
 loadIdentity
 frustum (-0.2)  0.2  (-0.15)  0.15  0.163  100.0
 matrixMode $= Modelview 0
 loadIdentity
 --texture Texture2D $= Enabled
 ListV shps <- dispPull
 mapM_ drawShape shps 
 --threadDelay $ 300*1000
 swapBuffers

unRealNum ::  RealNum-> GLdouble
unRealNum = realToFrac

fromPair3v (PairV (PairV x y) z) = (fromRational . toRational . unsafeVToDbl $ x, unsafeVToDbl $ y, unsafeVToDbl $ z)

drawShape :: V -> IO ()
--drawShape (BoxV (x1, y1) (x2, y2) (S.C r g b))
drawShape (BoxV shp loc col)
    = {-preservingMatrix $ -} do
        let (r, g,b) = fromPair3v col
        let (x0, y0, z0) = fromPair3v loc
        let (x, y, z) = fromPair3v shp
        color $ Color3 (unRealNum r) (unRealNum g) (unRealNum b) 
        translate $ Vector3 (unRealNum x0) (unRealNum y0) (unRealNum z0)
        scale (unRealNum x) (unRealNum y) (unRealNum z)
        rotate 90 $ Vector3 (0::GLfloat) 1 (0.0)

        renderPrimitive Quads $ unitCube
        
          --return ()

vertex3 :: GLfloat -> GLfloat -> GLfloat -> Vertex3 GLfloat
vertex3 x y z = Vertex3 x y z
texCoord3 :: GLfloat -> GLfloat -> GLfloat -> TexCoord3 GLfloat
texCoord3 x y z = TexCoord3 x y z

glTexCoord2f :: (GLfloat,GLfloat) -> IO ()
glTexCoord2f(x,y) = texCoord $ TexCoord2 x y 

glVertex3f ::(GLfloat,GLfloat, GLfloat) -> IO ()
glVertex3f (x,y,z) = vertex $ Vertex3 x y z 
--http://www.morrowland.com/apron/tutorials/gl/gl_rotating_cube.php
--http://nehe.gamedev.net/data/lessons/lesson.asp?lesson=06
unitCube = do
{-    drawPt 1.0 1.0 0.0	-- Top Right Of The Quad (Top)
    drawPt 0.0  1.0 0.0	-- Top Left Of The Quad (Top)
    drawPt 0.0  1.0  1.0	-- Bottom Left Of The Quad (Top)
    drawPt  1.0  1.0  1.0	-- Bottom Right Of The Quad (Top)

    drawPt  1.0 0.0  1.0	-- Top Right Of The Quad (Bottom)
    drawPt 0.0 0.0  1.0	-- Top Left Of The Quad (Bottom)
    drawPt 0.0 0.0 0.0	-- Bottom Left Of The Quad (Bottom)
    drawPt  1.0 0.0 0.0	-- Bottom Right Of The Quad (Bottom)

    drawPt  1.0  1.0  1.0	-- Top Right Of The Quad (Front)
    drawPt 0.0  1.0  1.0	-- Top Left Of The Quad (Front)
    drawPt 0.0 0.0  1.0	-- Bottom Left Of The Quad (Front)
    drawPt  1.0 0.0  1.0	-- Bottom Right Of The Quad (Front)

    drawPt  1.0 0.0 0.0	-- Top Right Of The Quad (Back)
    drawPt 0.0 0.0 0.0	-- Top Left Of The Quad (Back)
    drawPt 0.0  1.0 0.0	-- Bottom Left Of The Quad (Back)
    drawPt  1.0  1.0 0.0	-- Bottom Right Of The Quad (Back)

    drawPt 0.0  1.0  1.0	-- Top Right Of The Quad (Left)
    drawPt 0.0  1.0 0.0	-- Top Left Of The Quad (Left)
    drawPt 0.0 0.0 0.0	-- Bottom Left Of The Quad (Left)
    drawPt 0.0 0.0  1.0	-- Bottom Right Of The Quad (Left)

    drawPt  1.0  1.0 0.0	-- Top Right Of The Quad (Right)
    drawPt  1.0  1.0  1.0	-- Top Left Of The Quad (Right)
    drawPt  1.0 0.0  1.0	-- Bottom Left Of The Quad (Right)
    drawPt  1.0 0.0 0.0	-- Bottom Right Of The Quad (Right) -}
    let lC= 0.0
    glTexCoord2f(0.0, 0.0); glVertex3f(lC, lC,  1.0);	-- Bottom Left Of The Texture and Quad
    glTexCoord2f(1.0, 0.0); glVertex3f( 1.0, lC,  1.0);	-- Bottom Right Of The Texture and Quad
    glTexCoord2f(1.0, 1.0); glVertex3f( 1.0,  1.0,  1.0);	-- Top Right Of The Texture and Quad
    glTexCoord2f(0.0, 1.0); glVertex3f(lC,  1.0,  1.0);	-- Top Left Of The Texture and Quad
    -- Back Face
    glTexCoord2f(1.0, 0.0); glVertex3f(lC, lC, lC);	-- Bottom Right Of The Texture and Quad
    glTexCoord2f(1.0, 1.0); glVertex3f(lC,  1.0, lC);	-- Top Right Of The Texture and Quad
    glTexCoord2f(0.0, 1.0); glVertex3f( 1.0,  1.0, lC);	-- Top Left Of The Texture and Quad
    glTexCoord2f(0.0, 0.0); glVertex3f( 1.0, lC, lC);	-- Bottom Left Of The Texture and Quad
    -- Top Face
    glTexCoord2f(0.0, 1.0); glVertex3f(lC,  1.0, lC);	-- Top Left Of The Texture and Quad
    glTexCoord2f(0.0, 0.0); glVertex3f(lC,  1.0,  1.0);	-- Bottom Left Of The Texture and Quad
    glTexCoord2f(1.0, 0.0); glVertex3f( 1.0,  1.0,  1.0);	-- Bottom Right Of The Texture and Quad
    glTexCoord2f(1.0, 1.0); glVertex3f( 1.0,  1.0, lC);	-- Top Right Of The Texture and Quad
    -- Bottom Face
    glTexCoord2f(1.0, 1.0); glVertex3f(lC, lC, lC);	-- Top Right Of The Texture and Quad
    glTexCoord2f(0.0, 1.0); glVertex3f( 1.0, lC, lC);	-- Top Left Of The Texture and Quad
    glTexCoord2f(0.0, 0.0); glVertex3f( 1.0, lC,  1.0);	-- Bottom Left Of The Texture and Quad
    glTexCoord2f(1.0, 0.0); glVertex3f(lC, lC,  1.0);	-- Bottom Right Of The Texture and Quad
    -- Right face
    glTexCoord2f(1.0, 0.0); glVertex3f( 1.0, lC, lC);	-- Bottom Right Of The Texture and Quad
    glTexCoord2f(1.0, 1.0); glVertex3f( 1.0,  1.0, lC);	-- Top Right Of The Texture and Quad
    glTexCoord2f(0.0, 1.0); glVertex3f( 1.0,  1.0,  1.0);	-- Top Left Of The Texture and Quad
    glTexCoord2f(0.0, 0.0); glVertex3f( 1.0, lC,  1.0);	-- Bottom Left Of The Texture and Quad
    -- Left Face
    glTexCoord2f(0.0, 0.0); glVertex3f(lC, lC, lC);	-- Bottom Left Of The Texture and Quad
    glTexCoord2f(1.0, 0.0); glVertex3f(lC, lC,  1.0);	-- Bottom Right Of The Texture and Quad
    glTexCoord2f(1.0, 1.0); glVertex3f(lC,  1.0,  1.0);	-- Top Right Of The Texture and Quad
    glTexCoord2f(0.0, 1.0); glVertex3f(lC,  1.0, lC);


	
#else
initGlScreen _ _ _ _ = return ()
setTexture _ = return ()
#endif
