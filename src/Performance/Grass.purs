module Performace.Grass where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.ST.Class (liftST)
import Data.Array.ST as STA
import Data.ArrayBuffer.Typed as ArrayBuffer
import Data.ArrayBuffer.Types as ABT
import Data.Float32 as Float32
import Data.Number (cos, pi, sin)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Random (randomRange)
import Effect.Unsafe (unsafePerformEffect)
import Performace.SimplexNoise (noise2D)
import Prim.Row (class Union)
import React.Basic (JSX, empty)
import React.Basic.Hooks (Component, useMemo, useRef)
import React.Basic.Hooks as React
import React.R3F.Drei.Loaders (useTexture)
import React.R3F.Drei.Shaders (ShaderMaterialProps, shaderMaterial)
import React.R3F.Hooks (useFrame)
import React.R3F.Three.Constants (doubleSide)
import React.R3F.Three.Core (getAttribute, getElapsedTime, getIndex, instancedBufferAttribute, instancedBufferGeometry, lookAt, translate)
import React.R3F.Three.Internal (null)
import React.R3F.Three.Materials (meshStandardMaterial, setUniforms)
import React.R3F.Three.Math (normalize, set4, w, x, y, z) as Math
import React.R3F.Three.Math (vector4, vector4Default)
import React.R3F.Three.Objects (group, mesh)
import React.R3F.Three.Types (Color, Texture, createColor, createPlaneGeometry, createVector3)
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

type GrassParams =
  { bW :: Number
  , bH :: Number
  , joints :: Int
  }

mkGrass :: Component (GrassParams /\ Number /\ Int)
mkGrass = do
  darkGreen <- createColor "#000f00"

  React.component "grass" \(params /\ width /\ instances) -> React.do
    materialRef <- useRef empty
    -- The two textures have been taken from "Realistic real-time grass rendering" by Eddie Lee, 2010
    texture <- useTexture "/assets/textures/grass/blade_diffuse.jpg"
    alphaTexture <- useTexture "/assets/textures/grass/blade_alpha.jpg"

    offsets /\ orientations /\ stretches /\ halfRootAngleSin /\ halfRootAngleCos <- useMemo (width /\ instances)
      \_ -> unsafePerformEffect do
        a /\ b /\ c /\ d /\ e <- getAttributeData instances width
        (a' :: ABT.Float32Array) <- ArrayBuffer.fromArray $ map Float32.fromNumber' a
        (b' :: ABT.Float32Array) <- ArrayBuffer.fromArray $ map Float32.fromNumber' b
        (c' :: ABT.Float32Array) <- ArrayBuffer.fromArray $ map Float32.fromNumber' c
        (d' :: ABT.Float32Array) <- ArrayBuffer.fromArray $ map Float32.fromNumber' d
        (e' :: ABT.Float32Array) <- ArrayBuffer.fromArray $ map Float32.fromNumber' e
        pure $ a' /\ b' /\ c' /\ d' /\ e'

    baseIndex /\ basePosition /\ baseUV <- useMemo params \_ -> unsafePerformEffect do
      baseGeo <- createPlaneGeometry params.bW params.bH 1 params.joints
      translate baseGeo 0.0 (params.bH / 2.0) 0.0
      index <- getIndex baseGeo
      position <- getAttribute baseGeo "position"
      uv <- getAttribute baseGeo "uv"
      pure $ index /\ position /\ uv

    groundGeo <- useMemo width \_ -> unsafePerformEffect do
      geo <- createPlaneGeometry width width 32 32
      vector <- createVector3 [ 0.0, 1.0, 0.0 ]
      lookAt geo vector
      pure geo

    useFrame \state _ -> do
      elapsed <- getElapsedTime state.clock
      setUniforms materialRef "time" $ elapsed / 4.0

    let
      grass = mesh
        { children:
            [ instancedBufferGeometry
                { index: baseIndex
                , "attributes-position": basePosition
                , "attributes-uv": baseUV
                , children:
                    [ instancedBufferAttribute
                        { array: offsets, itemSize: 3 }
                        { attach: "attributes-offset" }
                    , instancedBufferAttribute
                        { array: orientations, itemSize: 4 }
                        { attach: "attributes-orientation" }
                    , instancedBufferAttribute
                        { array: stretches, itemSize: 1 }
                        { attach: "attributes-stretch" }
                    , instancedBufferAttribute
                        { array: halfRootAngleSin, itemSize: 1 }
                        { attach: "attributes-halfRootAngleSin" }
                    , instancedBufferAttribute
                        { array: halfRootAngleCos, itemSize: 1 }
                        { attach: "attributes-halfRootAngleCos" }
                    ]
                }
            , grassMaterial
                { ref: materialRef
                , map: texture
                , alphaMap: alphaTexture
                , toneMapped: false
                }
            ]
        }
      ground = mesh
        { position: [ 0.0, 0.0, 0.0 ]
        , geometry: groundGeo
        , children:
            [ meshStandardMaterial { color: darkGreen } ]
        }

    pure $ group { children: [ grass, ground ] }

  where

  getYPosition x z =
    2.0 * noise2D (x / 50.0) (z / 50.0)
      + 4.0 * noise2D (x / 100.0) (z / 100.0)
      + 0.2 * noise2D (x / 10.0) (z / 10.0)

  getAttributeData instances width = do
    offsets <- liftST $ STA.new
    orientations <- liftST $ STA.new
    stretches <- liftST $ STA.new
    halfRootAngleSin <- liftST $ STA.new
    halfRootAngleCos <- liftST $ STA.new

    quaternion0 <- vector4Default
    quaternion1 <- vector4Default

    let
      -- the min / max angle for the growth direction (in radians)
      min = -0.25
      max = 0.25

      multiplyQuaternions q1 q2 = do
        x1 <- Math.x q1
        y1 <- Math.y q1
        z1 <- Math.z q1
        w1 <- Math.w q1
        x2 <- Math.x q2
        y2 <- Math.y q2
        z2 <- Math.z q2
        w2 <- Math.w q2
        let
          x = x1 * w2 + y1 * z2 - z1 * y2 + w1 * x2
          y = -x1 * z2 + y1 * w2 + z1 * x2 + w1 * y2
          z = x1 * y2 - y1 * x2 + z1 * w2 + w1 * z2
          w = -x1 * x2 - y1 * y2 - z1 * z2 + w1 * w2
        vector4 x y z w

      go i | i >= instances = pure $ Done unit
      go i = do
        -- offsets of the roots
        offsetX <- randomRange (-width / 2.0) (width / 2.0)
        offsetZ <- randomRange (-width / 2.0) (width / 2.0)
        let
          offsetY = getYPosition offsetX offsetZ
        _ <- liftST $ STA.pushAll [ offsetX, offsetY, offsetZ ] offsets

        -- random growth direction
        -- rotate around Y
        angle <- randomRange (-pi) pi
        let
          sin_ = sin (0.5 * angle)
          cos_ = cos (0.5 * angle)
        _ <- liftST $ STA.push sin_ halfRootAngleSin
        _ <- liftST $ STA.push cos_ halfRootAngleCos
        Math.set4 quaternion0 0.0 sin_ 0.0 cos_
        Math.normalize quaternion0

        -- rotate around X
        angle' <- randomRange min max
        Math.set4 quaternion1 (sin (angle' / 2.0)) 0.0 0.0 (cos (angle' / 2.0))
        Math.normalize quaternion1
        quaternion0' <- multiplyQuaternions quaternion0 quaternion1
        x <- Math.x quaternion0'
        y <- Math.y quaternion0'
        z <- Math.z quaternion0'
        w <- Math.w quaternion0'
        _ <- liftST $ STA.pushAll [ x, y, z, w ] orientations

        -- define variant in height
        stretch <- if (i < instances / 3) then randomRange 0.0 1.8 else randomRange 0.0 1.0
        _ <- liftST $ STA.push stretch stretches

        pure $ Loop (i + 1)

    tailRecM go 0
    a <- liftST $ STA.unsafeFreeze offsets
    b <- liftST $ STA.unsafeFreeze orientations
    c <- liftST $ STA.unsafeFreeze stretches
    d <- liftST $ STA.unsafeFreeze halfRootAngleSin
    e <- liftST $ STA.unsafeFreeze halfRootAngleCos
    pure $ a /\ b /\ c /\ d /\ e

type GrassMatParams =
  ( bladeHeight :: Number
  , map :: Texture
  , alphaMap :: Texture
  , time :: Number
  , tipCodor :: Color
  , bottomColor :: Color
  )

grassMaterial
  :: forall props props_ a b c d
   . Union props props_ ((ShaderMaterialProps a b c d) + GrassMatParams)
  => { | props }
  -> JSX
grassMaterial props = unsafePerformEffect do
  tipColor <- createColor "#009900"
  bottomColor <- createColor "#002000"
  pure $ shaderMaterial
    @"GrassMaterial"
    { bladeHeight: 1.0
    , map: unsafeCoerce null
    , alphaMap: unsafeCoerce null
    , time: 0.0
    , tipColor
    , bottomColor
    , side: doubleSide
    }
    """
    precision mediump float;
    attribute vec3 offset;
    attribute vec4 orientation;
    attribute float halfRootAngleSin;
    attribute float halfRootAngleCos;
    attribute float stretch;
    uniform float time;
    uniform float bladeHeight;
    varying vec2 vUv;
    varying float frc;

    //WEBGL-NOISE FROM https://github.com/stegu/webgl-noise
    //Description : Array and textureless GLSL 2D simplex noise function. Author : Ian McEwan, Ashima Arts. Maintainer : stegu Lastmod : 20110822 (ijm) License : Copyright (C) 2011 Ashima Arts. All rights reserved. Distributed under the MIT License. See LICENSE file. https://github.com/ashima/webgl-noise https://github.com/stegu/webgl-noise
    vec3 mod289(vec3 x) {return x - floor(x * (1.0 / 289.0)) * 289.0;} vec2 mod289(vec2 x) {return x - floor(x * (1.0 / 289.0)) * 289.0;} vec3 permute(vec3 x) {return mod289(((x*34.0)+1.0)*x);} float snoise(vec2 v){const vec4 C = vec4(0.211324865405187, 0.366025403784439, -0.577350269189626, 0.024390243902439); vec2 i  = floor(v + dot(v, C.yy) ); vec2 x0 = v -   i + dot(i, C.xx); vec2 i1; i1 = (x0.x > x0.y) ? vec2(1.0, 0.0) : vec2(0.0, 1.0); vec4 x12 = x0.xyxy + C.xxzz; x12.xy -= i1; i = mod289(i); vec3 p = permute( permute( i.y + vec3(0.0, i1.y, 1.0 )) + i.x + vec3(0.0, i1.x, 1.0 )); vec3 m = max(0.5 - vec3(dot(x0,x0), dot(x12.xy,x12.xy), dot(x12.zw,x12.zw)), 0.0); m = m*m ; m = m*m ; vec3 x = 2.0 * fract(p * C.www) - 1.0; vec3 h = abs(x) - 0.5; vec3 ox = floor(x + 0.5); vec3 a0 = x - ox; m *= 1.79284291400159 - 0.85373472095314 * ( a0*a0 + h*h ); vec3 g; g.x  = a0.x  * x0.x  + h.x  * x0.y; g.yz = a0.yz * x12.xz + h.yz * x12.yw; return 130.0 * dot(m, g);}
    //END NOISE

    //https://www.geeks3d.com/20141201/how-to-rotate-a-vertex-by-a-quaternion-in-glsl/
    vec3 rotateVectorByQuaternion( vec3 v, vec4 q){
      return 2.0 * cross(q.xyz, v * q.w + cross(q.xyz, v)) + v;
    }

    //https://en.wikipedia.org/wiki/Slerp
    vec4 slerp(vec4 v0, vec4 v1, float t) {
      // Only unit quaternions are valid rotations.
      // Normalize to avoid undefined behavior.
      normalize(v0);
      normalize(v1);

      // Compute the cosine of the angle between the two vectors.
      float dot_ = dot(v0, v1);

      // If the dot product is negative, slerp won't take
      // the shorter path. Note that v1 and -v1 are equivalent when
      // the negation is applied to all four components. Fix by
      // reversing one quaternion.
      if (dot_ < 0.0) {
        v1 = -v1;
        dot_ = -dot_;
      }

      const float DOT_THRESHOLD = 0.9995;
      if (dot_ > DOT_THRESHOLD) {
        // If the inputs are too close for comfort, linearly interpolate
        // and normalize the result.
        vec4 result = t*(v1 - v0) + v0;
        normalize(result);
        return result;
      }

      // Since dot is in range [0, DOT_THRESHOLD], acos is safe
      float theta_0 = acos(dot_);       // theta_0 = angle between input vectors
      float theta = theta_0*t;          // theta = angle between v0 and result
      float sin_theta = sin(theta);     // compute this value only once
      float sin_theta_0 = sin(theta_0); // compute this value only once
      float s0 = cos(theta) - dot_ * sin_theta / sin_theta_0;  // == sin(theta_0 - theta) / sin(theta_0)
      float s1 = sin_theta / sin_theta_0;
      return (s0 * v0) + (s1 * v1);
    }

    void main() {
      //Relative position of vertex along the mesh Y direction
      frc = position.y/float(bladeHeight);
      //Get wind data from simplex noise
      float noise = 1.0-(snoise(vec2((time-offset.x/50.0), (time-offset.z/50.0))));
      //Define the direction of an unbent blade of grass rotated around the Y axis
      vec4 direction = vec4(0.0, halfRootAngleSin, 0.0, halfRootAngleCos);
      //Interpolate between the unbent direction and the direction of growth calculated on the CPU.
      //Using the relative location of the vertex along the Y axis as the weight, we get a smooth bend
      direction = slerp(direction, orientation, frc);
      vec3 vPosition = vec3(position.x, position.y + position.y * stretch, position.z);
      vPosition = rotateVectorByQuaternion(vPosition, direction);

     //Apply wind
     float halfAngle = noise * 0.15;
      vPosition = rotateVectorByQuaternion(vPosition, normalize(vec4(sin(halfAngle), 0.0, -sin(halfAngle), cos(halfAngle))));
      //UV for texture
      vUv = uv;
      //Calculate final position of the vertex from the world offset and the above shenanigans
      gl_Position = projectionMatrix * modelViewMatrix * vec4(offset + vPosition, 1.0 );
    }
    """
    """
    precision mediump float;
    uniform sampler2D map;
    uniform sampler2D alphaMap;
    uniform vec3 tipColor;
    uniform vec3 bottomColor;
    varying vec2 vUv;
    varying float frc;

    void main() {
      //Get transparency information from alpha map
      float alpha = texture2D(alphaMap, vUv).r;
      //If transparent, don't draw
      if(alpha < 0.15) discard;
      //Get colour data from texture
      vec4 col = vec4(texture2D(map, vUv));
      //Add more green towards root
      col = mix(vec4(tipColor, 1.0), col, frc);
      //Add a shadow towards root
      col = mix(vec4(bottomColor, 1.0), col, frc);
      gl_FragColor = col;

      #include <tonemapping_fragment>
      #include <encodings_fragment>
    }
    """
    props

