module Stories.Effect.FrostedGlass where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Number (cos, sin)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Exception as Exception
import Effect.FrostedGlassOverlay (mkOverlay)
import React.Basic.DOM as DOM
import React.Basic.DOM.Client as Client
import React.Basic.Hooks (Component, useState')
import React.Basic.Hooks as React
import React.Basic.StrictMode as StrictMode
import React.R3F.Drei.Loaders (useGLTF)
import React.R3F.Drei.Misc (perf)
import React.R3F.Drei.Shaders (meshTransmissionMaterial)
import React.R3F.Drei.Staging (contactShadows, environment)
import React.R3F.Hooks (useFrame)
import React.R3F.Three.Core (getElapsedTime)
import React.R3F.Three.Geometries (circleGeometry)
import React.R3F.Three.Lights (ambientLight, spotLight)
import React.R3F.Three.Objects (group, mesh)
import React.R3F.Web (canvas)
import Untagged.Castable (cast)
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
  jwst <- mkJWST
  overlay <- mkOverlay

  React.component "Canvas" \_ -> React.do
    showOverlay /\ set <- useState' false

    let
      cssOverlay = DOM.css
        { position: "fixed"
        , display: "flex"
        , width: "100%"
        , height: "100%"
        , top: 0
        , left: 0
        , right: 0
        , bottom: 0
        }
      selector = mesh
        { scale: if showOverlay then 5.0 else 0.01
        , position: [ 0.0, 0.0, 3.0 ]
        , children:
            [ circleGeometry
                { radius: 1.0 }
                {}
            , meshTransmissionMaterial
                { samples: 16
                , resolution: 512
                , anisotropicBlur: 0.1
                , thickness: 0.1
                , roughness: 0.4
                , toneMapped: true
                , color: if showOverlay then "#f0f0f0" else "#ccc"
                }
            ]
        }

    pure $ DOM.div
      { style: cssOverlay
      , children:
          [ canvas
              { camera: { position: [ 0.0, 0.0, 4.0 ], fov: 40.0 }
              , children:
                  [ perf { position: "bottom-right" }
                  , ambientLight { intensity: 0.7 }
                  , spotLight
                      {}
                      { castShadow: true
                      , position: [ 10.0, 15.0, -5.0 ]
                      , intensity: 0.5
                      , angle: 0.1
                      , penumbra: 1.0
                      }
                  , environment { preset: "night", background: cast true, blur: 1.0 }
                  , contactShadows
                      { resolution: 512
                      , position: [ 0.0, -0.8, 0.0 ]
                      , opacity: 1.0
                      , scale: cast 10.0
                      , blur: 2.0
                      , far: 0.8
                      }
                  , selector
                  , jwst set
                  ]
              }
          , overlay showOverlay
          ]
      }

mkJWST :: Component (Boolean -> Effect Unit)
mkJWST = React.component "JWST" \set -> React.do
  position /\ setPos <- useState' [ 0.0, 0.0, 0.0 ]
  rotation /\ setRot <- useState' [ 0.0, 0.0, 0.0 ]
  { nodes } <- useGLTF "/assets/gltf/jwst/jwst-transformed.glb"

  useFrame \state _ -> do
    t <- (_ / 4.0) <$> getElapsedTime state.clock
    setRot [ cos t / 8.0, sin t / 4.0, 0.15 + sin t / 8.0 ]
    setPos [ 0.0, -0.2 + cos t / 6.0, 0.0 ]

  pure $ group
    { scale: 0.07
    , position
    , rotation
    , onPointerOver: set true
    , onPointerOut: set false
    , children: [ mesh nodes."Object_8" ]
    }

