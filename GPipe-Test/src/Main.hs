{-# LANGUAGE ScopedTypeVariables, PackageImports, FlexibleContexts, TypeFamilies #-}
{-# LANGUAGE TupleSections, RankNTypes, ImpredicativeTypes #-}
module Main where

import Control.Applicative
import Control.Monad
import Text.Printf (printf)
import "transformers" Control.Monad.IO.Class

import Graphics.GPipe
import qualified "GPipe-GLFW" Graphics.GPipe.Context.GLFW as GLFW
import qualified "JuicyPixels" Codec.Picture as Juicy
import qualified "JuicyPixels" Codec.Picture.Types as Juicy
import "linear" Linear
import Data.IORef
import GHC.Float 
import Data.Either
import Data.Int
import Control.Lens
import qualified Data.List as List
import Control.Arrow (first)

type MAPUF os = Buffer os (Uniform (V4 (B4 Float), V4 (B4 Float), V4 (B4 Float), V3 (B3 Float)))
type CAMUF os = Buffer os (Uniform (V3 (B Float), B Float, B Float))

main :: IO ()
main = runContextT GLFW.newContext (ContextFormatColorDepth SRGB8 Depth16) $ do
    -- Spew scroll info
    cr <- liftIO $ newIORef 5
    GLFW.registerScrollCallback . pure $
        \w dx dy -> do 
          -- printf "scroll dx%v dy%v on %v\n" dx dy (show w)
          modifyIORef cr (0.5*dy+)

    -- Create uniforms 
    mapUF :: Buffer os (Uniform (V4 (B4 Float), V4 (B4 Float), V4 (B4 Float), V3 (B3 Float))) <- newBuffer 1
    camUF :: Buffer os (Uniform (V3 (B Float), B Float, B Float)) <- newBuffer 1

    -- Create texes
    texes <- makeTexes

    -- Create render and shader
    renders <- sequence [
        makeCubeRender $ cubeShader mapUF camUF texes
      , makeAxisRender $ axisShader mapUF
      ]

    -- Run the loop
    loop renders mapUF camUF (0.0, cr, 0, pi / 2, 5, 2)

loop renders mapUF camUF params = do
  (params'@(angle, r,a,b,lr,ls), size@(V2 w h)) <- update params
  -- Render the frame and present the results
  render $ do
    clearContextColor 0 -- Black
    clearContextDepth 1 -- Far plane
    mapM_ ($size) renders 
  swapContextBuffers

  closeRequested <- GLFW.windowShouldClose
  unless closeRequested $
    loop renders mapUF camUF params'
  where
    update (angle, r,a,b,lr,ls) = do
      (cursorX, cursorY) <- GLFW.getCursorPos
      mouseButton1 <- GLFW.getMouseButton GLFW.MouseButton'1
      spaceKey <- GLFW.getKey GLFW.Key'Space
      leftKey <- GLFW.getKey GLFW.Key'Left
      rightKey <- GLFW.getKey GLFW.Key'Right
      upKey <- GLFW.getKey GLFW.Key'Up
      downKey <- GLFW.getKey GLFW.Key'Down
      minusKey <- GLFW.getKey GLFW.Key'PadSubtract
      plusKey <- GLFW.getKey GLFW.Key'PadAdd
      pgdKey <- GLFW.getKey GLFW.Key'PageDown
      pguKey <- GLFW.getKey GLFW.Key'PageUp
      shouldClose <- GLFW.windowShouldClose
      
      -- liftIO $ printf "cursorPos x%v y%v, mouseButton1 %v, spaceKey %v, shouldClose %v\n"
      --   cursorX cursorY (show mouseButton1) (show spaceKey) (show shouldClose)

      -- calculate camera coord
      let lr' = case pgdKey of 
                    GLFW.KeyState'Pressed -> lr - 0.001
                    _ -> case pguKey of
                      GLFW.KeyState'Pressed -> lr + 0.001
                      _ -> lr
      let ls' = case minusKey of 
                    GLFW.KeyState'Pressed -> ls + 2
                    _ -> case plusKey of
                      GLFW.KeyState'Pressed -> abs (ls - 2)
                      _ -> ls
      let a' = case leftKey of 
                    GLFW.KeyState'Pressed -> a - 0.001
                    _ -> case rightKey of
                      GLFW.KeyState'Pressed -> a + 0.001
                      _ -> a
      let b' = case downKey of 
                    GLFW.KeyState'Pressed -> b - 0.001
                    _ -> case upKey of
                      GLFW.KeyState'Pressed -> b + 0.001
                      _ -> b
      r' <- liftIO . fmap double2Float $ readIORef r
      let z = sin b' * r'
          x = cos b' * r' * cos a'
          y = cos b' * r' * sin a'

      -- Write this frames mapUF value
      size@(V2 w h) <- getContextBuffersSize
      let modelRot = fromQuaternion (axisAngle (V3 1 1 1) angle)
          modelMat = mkTransformationMat modelRot (pure 0) !!* 0.5
          projMat = perspective (pi/2) (fromIntegral w / fromIntegral h) 1 100
          campos = (V3 x y z)
          camup = (V3 0 0 (norm campos / cos b') - campos)
          viewMat2 = lookAt campos (V3 0 0 0) camup
          normRot = modelRot
      writeBuffer mapUF 0 [(modelMat, viewMat2, projMat, normRot)]
      writeBuffer camUF 0 [(campos, lr', ls')]
      return ((((angle + 0.0001) `mod''` (2*pi)), r, a',b',lr',ls'), size)

makeCubeRender shader = do 
  positions :: Buffer os (B2 Float) <- newBuffer 4
  normals   :: Buffer os (B3 Float) <- newBuffer 6
  tangents  :: Buffer os (B3 Float) <- newBuffer 6
  sampsIdx  :: Buffer os (B Int32) <- newBuffer 6
  writeBuffer positions 0 [V2 1 1, V2 1 (-1), V2 (-1) 1, V2 (-1) (-1)]
  writeBuffer normals 0 [V3 1 0 0, V3 (-1) 0 0, V3 0 1 0, V3 0 (-1) 0, V3 0 0 1, V3 0 0 (-1)]
  writeBuffer tangents 0 [V3 0 1 0, V3 0 (-1) 0, V3 0 0 1, V3 0 0 (-1), V3 (-1) 0 0, V3 1 0 0]
  writeBuffer sampsIdx 0 [1..6]
  -- Make a Render action that create a PrimitiveArray and call shader for it
  s <- compileShader shader
  return $ \ ws -> do
    pArr <- newVertexArray positions
    nArr <- newVertexArray normals
    tArr <- newVertexArray tangents
    sArr <- newVertexArray sampsIdx
    let sideInstances = zipVertices (\(a,b) c -> (a,b,c)) (zipVertices (,) nArr tArr) sArr
    s . ShaderEnvironment (FrontAndBack, ViewPort 0 ws, DepthRange 0 1) $ toPrimitiveArrayInstanced TriangleStrip (,) pArr sideInstances

cubeShader (mapUF :: MAPUF os) (camUF :: CAMUF os) texes = do
  cube <- fmap makeSide <$> toPrimitiveStream primitiveArray
  (modelMat, viewMat, projMat, normMat) <- getUniform (const (mapUF, 0))
  let modelViewProj = projMat !*! viewMat !*! modelMat
      filterMode = SamplerFilter Linear Linear Linear (Just 4)
      edgeMode = (pure ClampToEdge, undefined)
      projectedCube = proj modelViewProj modelMat normMat <$> cube
  samps <- mapM (\tex -> newSampler2D (const (tex, filterMode, edgeMode))) texes
  (campos, lr, ls)<- getUniform (const (camUF, 0))
  fragNormalsUV <- rasterize rasterOptions projectedCube
  let -- litFrags = light samps (V3 0 (- lr) 0) <$> fragNormalsUV
      litFrags' = withRasterizedInfo (light2 samps (V3 0 0 lr) campos ls) fragNormalsUV
      litFragsWithDepth = withRasterizedInfo
          (\a x -> (a, rasterizedFragCoord x ^. _z)) litFrags'
      colorOption = ContextColorOption NoBlending (pure True)
      depthOption = DepthOption Less True
  drawContextColorDepth (const (colorOption, depthOption)) litFragsWithDepth

makeTexes = do
  let texImgs = fmap ((++".jpg") . show) [1..6]
  -- Load image into texture
  ims <- liftIO $ mapM Juicy.readJpeg texImgs
  let (_, jpgs) = partitionEithers ims
      images = (\(Juicy.ImageYCbCr8 image) -> image) <$> take 6 (concat . repeat $ jpgs)
      sizes = fmap (\image -> V2 (Juicy.imageWidth image) (Juicy.imageHeight image)) images
      imageWithSizes = zip images sizes
  texes <- mapM (\size -> newTexture2D SRGB8 size maxBound) sizes
  let texesWithSize = zip texes imageWithSizes
  forM_ texesWithSize $ \(tex, (image, size)) -> do
    writeTexture2D tex 0 0 size $ Juicy.pixelFold getJuicyPixel [] image
    generateTexture2DMipmap tex
  return texes

makeAxisRender shader = do
  axis :: Buffer os (B4 Float, B3 Float) <- newBuffer 6
  writeBuffer axis 0 $ zip
    (concat . List.transpose $ [replicate 3 (V4 0 0 0 1), [V4 1 0 0 1, V4 0 1 0 1, V4 0 0 1 1]])
    (concat . List.transpose $ replicate 2 [V3 1 0 0, V3 0 1 0, V3 0 0 1])
  s <- compileShader shader
  return $ \ ws -> do 
    aArr <- newVertexArray axis
    s . ShaderEnvironment (FrontAndBack, ViewPort 0 ws, DepthRange 0 1) $ toPrimitiveArray LineList aArr

axisShader (mapUF :: MAPUF os) = do
  axis <- toPrimitiveStream primitiveArray
  (_, viewMat, projMat, _) <- getUniform (const (mapUF, 0))
  let projectedAxis = first (projMat !*! viewMat !*) <$> axis
  fragAxis <- rasterize rasterOptions projectedAxis
  drawContextColor (const (ContextColorOption NoBlending (pure True))) fragAxis

getJuicyPixel :: forall t t1 a. Juicy.ColorSpaceConvertible a Juicy.PixelRGB8 => [V3 Juicy.Pixel8] -> t -> t1 -> a -> [V3 Juicy.Pixel8]
getJuicyPixel xs _x _y pix =
  let Juicy.PixelRGB8 r g b = Juicy.convertPixel pix in V3 r g b : xs

data ShaderEnvironment t b = ShaderEnvironment
  { rasterOptions :: (Side, ViewPort, DepthRange)
  , primitiveArray :: PrimitiveArray t b
  }

-- Project the sides coordinates using the instance's normal and tangent
makeSide :: (V2 VFloat, (V3 VFloat, V3 VFloat, VInt)) -> (V3 VFloat, V3 VFloat, V2 VFloat, VInt)
makeSide (p@(V2 x y), (normal, tangent, sampi)) =
  (V3 x y 1 *! V3 tangent bitangent normal, normal, uv, sampi)
  where bitangent = cross normal tangent
        uv = (p + 1) / 2

-- maxB' :: forall a. (IfB a, OrdB a) => a -> a -> a
-- maxB' a b = ifB (a <=* b) b a -- maxB causes frag link error when declare type signature. 

-- Project the cube's positions and normals with ModelViewProjection matrix
proj :: M44 VFloat -> M44 VFloat -> M33 VFloat -> (V3 VFloat, V3 VFloat, V2 VFloat, VInt) -> (V4 VFloat, (V3 FlatVFloat, V4 VFloat, V2 VFloat, VInt))
proj modelViewProj modelMat normMat (V3 px py pz, normal, uv, sampi) =
  (modelViewProj !* V4 px py pz 1, (fmap Flat $ normMat !* normal, modelMat !* V4 px py pz 1, uv, sampi))

-- Set color from sampler and apply directional light
light :: [Sampler2D (Format RGBFloat)] -> (V3 FFloat) -> (V3 FFloat, V4 FFloat, V2 FFloat, FInt) ->  ColorSample F RGBFloat
light samps lpos (normal, mpos, uv, sampi) =
  ifB (sampi ==* 1) (sample2D (head samps) SampleAuto Nothing Nothing uv * diffuse)
    $ ifB (sampi ==* 2) (sample2D (samps!!1) SampleAuto Nothing Nothing uv * diffuse)
      $ ifB (sampi ==* 3) (sample2D (samps!!2) SampleAuto Nothing Nothing uv * diffuse)
        $ ifB (sampi ==* 4) (sample2D (samps!!3) SampleAuto Nothing Nothing uv * diffuse)
          $ ifB (sampi ==* 5) (sample2D (samps!!4) SampleAuto Nothing Nothing uv * diffuse)
            (sample2D (samps!!5) SampleAuto Nothing Nothing uv * diffuse)
  where
    diffuse = pure . maxB 0 $ normal `dot` lpos

normalize' :: forall a (f :: * -> *). (Floating a, Metric f) => f a -> f a
normalize' vec = vec ^/ norm vec

light2 :: [Sampler2D (Format RGBFloat)] -> (V3 FFloat) -> (V3 FFloat) -> FFloat -> (V3 FFloat, V4 FFloat, V2 FFloat, FInt) -> RasterizedInfo -> ColorSample F RGBFloat
light2 samps lpos cpos shininess (normal, mpos, uv, sampi) RasterizedInfo {rasterizedFragCoord = ppos} =
  ifB (sampi ==* 1) (sample2D (head samps) SampleAuto Nothing Nothing uv * lt)
    $ ifB (sampi ==* 2) (sample2D (samps!!1) SampleAuto Nothing Nothing uv * lt)
      $ ifB (sampi ==* 3) (sample2D (samps!!2) SampleAuto Nothing Nothing uv * lt)
        $ ifB (sampi ==* 4) (sample2D (samps!!3) SampleAuto Nothing Nothing uv * lt)
          $ ifB (sampi ==* 5) (sample2D (samps!!4) SampleAuto Nothing Nothing uv * lt)
            (sample2D (samps!!5) SampleAuto Nothing Nothing uv * lt)
  where nsur2l = normalize' (lpos - (mpos ^. _xyz))
        sur2c = cpos - (mpos ^. _xyz)
        nsur2c = normalize' sur2c
        diffuse = maxB 0 $ normal `dot` nsur2l
        specular = ifB (diffuse ==* 0) 0 $ maxB 0 (nsur2c `dot` (- reflect normal nsur2l)) ** shininess
        dist = norm sur2c
        lt = pure $ 0.01 + 0.99 * diffuse / dist + specular 
        -- lt = pure $ 1
        -- lt = pure $ 0.005 + 0.995 * diffuse / dist
        -- lt = pure $ specular

reflect :: forall a (f :: * -> *). (Num a, Metric f) => f a -> f a -> f a
reflect pivot vec = vec ^-^ (vec ^* (2 * (vec `dot` pivot))) 

-- eof
