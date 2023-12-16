module Stories.Performance.SoManyCubes where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.ST.Class (liftST)
import Data.Array.ST as STA
import Data.ArrayBuffer.Typed as AB
import Data.ArrayBuffer.Types as ABT
import Data.Float32 as Float32
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Number (pi, pow)
import Effect (Effect)
import Effect.Exception as Exception
import Effect.Random (randomRange)
import Effect.Unsafe (unsafePerformEffect)
import Prim.Row (class Union)
import React.Basic (JSX, empty)
import React.Basic.DOM.Client as Client
import React.Basic.Hooks (Component, useLayoutEffect, useMemo, useRef)
import React.Basic.Hooks as React
import React.Basic.StrictMode as StrictMode
import React.R3F.Drei.Controls (orbitControls)
import React.R3F.Drei.Misc (perf)
import React.R3F.Drei.Shaders (ShaderMaterialProps, shaderMaterial)
import React.R3F.Drei.Staging (stage)
import React.R3F.Hooks (applyProps)
import React.R3F.Three.Core (getMatrix, instancedBufferAttribute, setPosition, setRotation, updateMatrix)
import React.R3F.Three.Geometries (boxGeometry)
import React.R3F.Three.Materials (meshLambertMaterial)
import React.R3F.Three.Objects (getGeometry, getInstanceMatrix, group, instancedMesh, setGeometry, setMatrixAt)
import React.R3F.Three.Types (Color, Vector3)
import React.R3F.Three.Types as Three
import React.R3F.Web (canvas)
import Type.Row (type (+))
import Web.DOM.NonElementParentNode as NonElementParentNode
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window

main :: Effect Unit
main = do
  maybeRoot <- HTML.window
    >>= Window.document
    >>= HTMLDocument.toNonElementParentNode
      >>> NonElementParentNode.getElementById "root"
  case maybeRoot of
    Nothing -> Exception.throw "Root element not found."
    Just root -> do
      app <- mkApp
      reactRoot <- Client.createRoot root
      Client.renderRoot reactRoot (StrictMode.strictMode (app unit))

mkApp :: Component Unit
mkApp = do
  cubes <- mkCubes

  React.component "" \_ -> React.do
    pure $ canvas
      { shadows: "soft"
      , children:
          [ orbitControls {}
          , perf { position: "bottom-right" }
          , stage { children: [ cubes 100_000 ] }
          ]
      }

mkCubes :: Component Int
mkCubes = do
  let
    mat = meshLambertMaterial
      {}
      { attach: "material", vertexColors: true, toneMapped: false }
  edgeColor <- Three.createColor "black"
  edgeSize <- Three.createVector3 [ 0.15, 0.15, 0.15 ]
  edgeMat <- meshEdgeMaterial
    { transparent: true
    , polygonOffset: true
    , polygonOffsetFactor: -10
    , color: edgeColor
    , size: edgeSize
    , thickness: 0.001
    , smoothness: 0.005
    }

  React.component "" \count -> React.do
    ref <- useRef empty
    outlines <- useRef empty

    (colors :: ABT.Float32Array) <- useMemo count \_ -> unsafePerformEffect do
      acc <- liftST $ STA.new
      let
        go 0 = pure $ Done acc
        go n = do
          r <- randomRange 0.0 1.0
          g <- randomRange 0.0 1.0
          b <- randomRange 0.0 1.0
          void $ liftST $ STA.pushAll (map Float32.fromNumber' [ r, g, b ]) acc
          pure $ Loop (n - 1)
      tailRecM go count >>= STA.unsafeFreeze >>> liftST >>= AB.fromArray

    useLayoutEffect count do
      obj <- Three.createObject3D
      let
        boundary = pow (toNumber count) 0.33333333 / 2.0

        go 0 = pure $ Done unit
        go i = do
          x <- randomRange (-boundary) boundary
          y <- randomRange (-boundary) boundary
          z <- randomRange (-boundary) boundary
          xr <- randomRange 0.0 (pi * 2.0)
          yr <- randomRange 0.0 (pi * 2.0)
          zr <- randomRange 0.0 (pi * 2.0)
          setPosition obj \_ _ _ -> [ x, y, z ]
          setRotation obj \_ _ _ -> [ xr, yr, zr ]
          updateMatrix obj
          getMatrix obj >>= setMatrixAt ref (i - 1)
          pure $ Loop (i - 1)

      tailRecM go count
      applyProps ref { "instanceMatrix-needsUpdate": true }
      getGeometry ref >>= setGeometry outlines
      getInstanceMatrix ref >>= \attr -> applyProps outlines { instanceMatrix: attr }
      pure mempty

    let
      cubes = instancedMesh
        { count }
        { ref
        , children:
            [ boxGeometry
                { width: 0.15, height: 0.15, depth: 0.15 }
                { attach: "geometry"
                , children:
                    [ instancedBufferAttribute
                        { array: colors, itemSize: 3 }
                        { attach: "attributes-color" }
                    ]
                }
            , mat
            ]
        }
      cubeOutlines = instancedMesh
        { count }
        { ref: outlines
        , children: [ edgeMat ]
        }
    pure $ group { children: [ cubes, cubeOutlines ] }

type EdgeMatParams =
  ( color :: Color
  , size :: Vector3
  , thickness :: Number
  , smoothness :: Number
  )

meshEdgeMaterial
  :: forall props props_ a b c d
   . Union props props_ ((ShaderMaterialProps a b c d) + EdgeMatParams)
  => { | props }
  -> Effect JSX
meshEdgeMaterial = \props -> do
  defaultColor <- Three.createColor "white"
  defaultSize <- Three.createVector3 [ 1.0, 1.0, 1.0 ]
  pure $ shaderMaterial
    @"MeshEdgeMaterial"
    { color: defaultColor
    , size: defaultSize
    , thickness: 0.01
    , smoothness: 0.2
    }
    """
    varying vec3 vPosition;
    void main() {
      vPosition = position;
      gl_Position = projectionMatrix * viewMatrix * instanceMatrix * vec4(position, 1.0);
    }
    """
    """
    varying vec3 vPosition;
    uniform vec3 size;
    uniform vec3 color;
    uniform float thickness;
    uniform float smoothness;
    void main() {
      vec3 d = abs(vPosition) - (size * 0.5);
      float a = smoothstep(thickness, thickness + smoothness, min(min(length(d.xy), length(d.yz)), length(d.xz)));
      gl_FragColor = vec4(color, 1.0 - a);
    }
    """
    props

