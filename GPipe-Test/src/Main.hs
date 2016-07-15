{-# LANGUAGE ScopedTypeVariables, PackageImports, FlexibleContexts, TypeFamilies #-}
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

main = do 
  z' <- newIORef 10
  runContextT GLFW.newContext (ContextFormatColorDepth SRGB8 Depth16) $ do
    -- Create vertex data buffers
    positions :: Buffer os (B2 Float) <- newBuffer 4
    normals   :: Buffer os (B3 Float) <- newBuffer 6
    tangents  :: Buffer os (B3 Float) <- newBuffer 6
    sampsIdx  :: Buffer os (B Int32) <- newBuffer 6
    writeBuffer positions 0 [V2 1 1, V2 1 (-1), V2 (-1) 1, V2 (-1) (-1)]
    writeBuffer normals 0 [V3 1 0 0, V3 (-1) 0 0, V3 0 1 0, V3 0 (-1) 0, V3 0 0 1, V3 0 0 (-1)]
    writeBuffer tangents 0 [V3 0 1 0, V3 0 (-1) 0, V3 0 0 1, V3 0 0 (-1), V3 (-1) 0 0, V3 1 0 0]
    writeBuffer sampsIdx 0 [1..6]

    -- Spew scroll info
    GLFW.registerScrollCallback . pure $
        \w dx dy -> do 
          printf "scroll dx%v dy%v on %v\n" dx dy (show w)
          modifyIORef z' (dy+)

    let texImgs = ["1.jpg", "2.jpg", "3.jpg", "4.jpg", "5.jpg", "6.jpg"]
    
    -- Load image into texture
    ims <- liftIO $ mapM Juicy.readJpeg texImgs
    let (fails, jpgs) = partitionEithers ims
        images = (\(Juicy.ImageYCbCr8 image) -> image) <$> take 6 (concat . repeat $ jpgs)
        sizes = fmap (\image -> V2 (Juicy.imageWidth image) (Juicy.imageHeight image)) images
        imageWithSizes = zip images sizes

    -- im <- pure $ Left "image.jpg"
    -- let image = case im of
    --         Right (Juicy.ImageYCbCr8 i) -> i
    --         Left s -> error s
    --     size = V2 (Juicy.imageWidth image) (Juicy.imageHeight image)
    
    texes <- mapM (\size -> newTexture2D SRGB8 size maxBound) sizes
    let texesWithSize = zip texes imageWithSizes
    forM_ texesWithSize $ \(tex, (image, size)) -> do
      writeTexture2D tex 0 0 size $ Juicy.pixelFold getJuicyPixel [] image
      generateTexture2DMipmap tex

    -- Make a Render action that returns a PrimitiveArray for the cube
    let makePrimitives = do
          pArr <- newVertexArray positions
          nArr <- newVertexArray normals
          tArr <- newVertexArray tangents
          sArr <- newVertexArray sampsIdx
          let sideInstances = zipVertices (\(a,b) c -> (a,b,c)) (zipVertices (,) nArr tArr) sArr
          return $ toPrimitiveArrayInstanced TriangleStrip (,) pArr sideInstances
      
    -- tex <- newTexture2D SRGB8 size maxBound -- JPG converts to SRGB
    -- writeTexture2D tex 0 0 size $ Juicy.pixelFold getJuicyPixel [] image
    -- generateTexture2DMipmap tex

    -- Create a buffer for the uniform values
    uniform :: Buffer os (Uniform (V4 (B4 Float), V3 (B3 Float))) <- newBuffer 1

    -- Create the shader
    shader <- compileShader $ do
      sides <- fmap makeSide <$> toPrimitiveStream primitives
      (modelViewProj, normMat) <- getUniform (const (uniform, 0))
      let filterMode = SamplerFilter Linear Linear Linear (Just 4)
          edgeMode = (pure ClampToEdge, undefined)
          projectedSides = proj modelViewProj normMat <$> sides
      samps <- mapM (\tex -> newSampler2D (const (tex, filterMode, edgeMode))) texes

      fragNormalsUV <- rasterize rasterOptions projectedSides

      let litFrags = light samps <$> fragNormalsUV
          litFragsWithDepth = withRasterizedInfo
              (\a x -> (a, getZ $ rasterizedFragCoord x)) litFrags
          colorOption = ContextColorOption NoBlending (pure True)
          depthOption = DepthOption Less True

      drawContextColorDepth (const (colorOption, depthOption)) litFragsWithDepth

    -- Run the loop
    loop shader makePrimitives uniform 0 z' 0 0

loop shader makePrimitives uniform angle z' x y = do
  (cursorX, cursorY)<- GLFW.getCursorPos
  mouseButton1 <- GLFW.getMouseButton GLFW.MouseButton'1
  spaceKey <- GLFW.getKey GLFW.Key'Space
  leftKey <- GLFW.getKey GLFW.Key'Left
  rightKey <- GLFW.getKey GLFW.Key'Right
  upKey <- GLFW.getKey GLFW.Key'Up
  downKey <- GLFW.getKey GLFW.Key'Down
  let newX = case leftKey of 
                GLFW.KeyState'Pressed -> x - 0.1
                _ -> case rightKey of
                  GLFW.KeyState'Pressed -> x + 0.1
                  _ -> x
  let newY = case downKey of 
                GLFW.KeyState'Pressed -> y - 0.1
                _ -> case upKey of
                  GLFW.KeyState'Pressed -> y + 0.1
                  _ -> y
  z <- liftIO $ readIORef z'
  shouldClose <- GLFW.windowShouldClose
  liftIO $ printf "cursorPos x%v y%v, mouseButton1 %v, spaceKey %v, shouldClose %v\n"
    cursorX cursorY (show mouseButton1) (show spaceKey) (show shouldClose)
  -- Write this frames uniform value
  size@(V2 w h) <- getContextBuffersSize
  let modelRot = fromQuaternion (axisAngle (V3 1 1 1) angle)
      modelMat = mkTransformationMat modelRot (pure 0)
      projMat = perspective (pi/2) (fromIntegral w / fromIntegral h) 1 100
      viewMat = mkTransformationMat identity (V3 x y (- double2Float z))
      viewProjMat = projMat !*! viewMat !*! modelMat
      normMat = modelRot
  writeBuffer uniform 0 [(viewProjMat, normMat)]

  -- Render the frame and present the results
  render $ do
    clearContextColor 0 -- Black
    clearContextDepth 1 -- Far plane
    prims <- makePrimitives
    shader $ ShaderEnvironment prims (FrontAndBack, ViewPort 0 size, DepthRange 0 1)
  swapContextBuffers

  closeRequested <- GLFW.windowShouldClose
  unless closeRequested $
    loop shader makePrimitives uniform ((angle + 0.01) `mod''` (2*pi)) z' newX newY

getJuicyPixel xs _x _y pix =
  let Juicy.PixelRGB8 r g b = Juicy.convertPixel pix in V3 r g b : xs

getZ (V4 _ _ z _) = z -- Some day I'll learn to use lenses instead...

data ShaderEnvironment = ShaderEnvironment
  { primitives :: PrimitiveArray Triangles (B2 Float, (B3 Float, B3 Float, B Int32))
  , rasterOptions :: (Side, ViewPort, DepthRange)
  }

-- Project the sides coordinates using the instance's normal and tangent
makeSide (p@(V2 x y), (normal, tangent, sampi)) =
  (V3 x y 1 *! V3 tangent bitangent normal, normal, uv, sampi)
  where bitangent = cross normal tangent
        uv = (p + 1) / 2

-- Project the cube's positions and normals with ModelViewProjection matrix
proj modelViewProj normMat (V3 px py pz, normal, uv, sampi) =
  (modelViewProj !* V4 px py pz 1, (fmap Flat $ normMat !* normal, uv, sampi))

-- Set color from sampler and apply directional light
light samps (normal, uv, sampi) =
  ifB (sampi ==* 1) (sample2D (samps!!0) SampleAuto Nothing Nothing uv * pure (normal `dot` V3 0 0 1))
    $ ifB (sampi ==* 2) (sample2D (samps!!1) SampleAuto Nothing Nothing uv * pure (normal `dot` V3 0 0 1))
      $ ifB (sampi ==* 3) (sample2D (samps!!2) SampleAuto Nothing Nothing uv * pure (normal `dot` V3 0 0 1))
        $ ifB (sampi ==* 4) (sample2D (samps!!3) SampleAuto Nothing Nothing uv * pure (normal `dot` V3 0 0 1))
          $ ifB (sampi ==* 5) (sample2D (samps!!4) SampleAuto Nothing Nothing uv * pure (normal `dot` V3 0 0 1))
            (sample2D (samps!!5) SampleAuto Nothing Nothing uv * pure (normal `dot` V3 0 0 1))

-- eof
