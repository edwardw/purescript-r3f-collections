module Stories.Performace.SoManyCubes where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array ((..))
import Data.ArrayBuffer.Typed as AB
import Data.ArrayBuffer.Types as ABT
import Data.Float32 as Float32
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Number (pi, pow)
import Effect (Effect)
import Effect.Exception as Exception
import Effect.Random (randomRange)
import Effect.Unsafe (unsafePerformEffect)
import React.Basic (empty)
import React.Basic.DOM.Client as Client
import React.Basic.Hooks (Component, useLayoutEffect, useMemo, useRef)
import React.Basic.Hooks as React
import React.Basic.StrictMode as StrictMode
import React.R3F.Drei.Controls (orbitControls)
import React.R3F.Drei.Staging (stage)
import React.R3F.Hooks (applyProps)
import React.R3F.Three.Core (instancedBufferAttribute, matrix, updateMatrix)
import React.R3F.Three.Geometries (boxGeometry)
import React.R3F.Three.Materials (meshLambertMaterial)
import React.R3F.Three.Objects (instancedMesh, setMatrixAt)
import React.R3F.Three.Types as Three
import React.R3F.Web (canvas)
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
  let
    orbitControl = orbitControls {}
  cubes <- mkCubes

  React.component "" \_ -> React.do
    pure $ canvas
      { shadows: "soft"
      , children:
          [ orbitControl
          , stage { children: [ cubes 10_000 ] }
          ]
      }

mkCubes :: Component Int
mkCubes = do
  let
    mat =
      meshLambertMaterial
        {}
        { attach: "material", vertexColors: true, toneMapped: false }

  React.component "" \count -> React.do
    ref <- useRef empty

    (colors :: ABT.Float32Array) <- useMemo count \_ -> do
      let
        go { acc, i: 0 } = pure $ Done acc
        go { acc, i } | i < 0 = pure $ Done acc
        go { acc, i } = do
          r <- randomRange 0.0 1.0
          g <- randomRange 0.0 1.0
          b <- randomRange 0.0 1.0
          pure $ Loop { acc: acc <> map Float32.fromNumber' [ r, g, b ], i: i - 1 }
      tailRecM go { acc: [], i: count } >>= AB.fromArray # unsafePerformEffect

    useLayoutEffect count do
      let
        boundary = pow (toNumber count) 0.33333333 / 2.0
        ix = 0 .. (count - 1)

      obj <- Three.createObject3D
      for_ ix \i -> do
        x <- randomRange (-boundary) boundary
        y <- randomRange (-boundary) boundary
        z <- randomRange (-boundary) boundary
        xr <- randomRange 0.0 (pi * 2.0)
        yr <- randomRange 0.0 (pi * 2.0)
        zr <- randomRange 0.0 (pi * 2.0)
        applyProps obj { position: [ x, y, z ] }
        applyProps obj { rotation: [ xr, yr, zr ] }
        updateMatrix obj
        matrix obj >>= setMatrixAt ref i
      applyProps ref { "instanceMatrix-needsUpdate": true }
      pure mempty

    pure $ instancedMesh
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

