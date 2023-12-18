module Stories.Shaders.RoomWithSoftShadows where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Number (pi)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Exception as Exception
import React.Basic.DOM.Client as Client
import React.Basic.Hooks (Component, useEffectOnce, useState)
import React.Basic.Hooks as React
import React.Basic.StrictMode as StrictMode
import React.R3F.Drei.Controls (cameraControls)
import React.R3F.Drei.Controls.LilGUI as LilGUI
import React.R3F.Drei.Loaders (useGLTF)
import React.R3F.Drei.Misc (perf)
import React.R3F.Drei.Shaders (softShadows)
import React.R3F.Drei.Staging (float, sky)
import React.R3F.Three.Cameras (orthographicCamera)
import React.R3F.Three.Geometries (sphereGeometry)
import React.R3F.Three.Lights (ambientLight, directionalLight)
import React.R3F.Three.Materials (meshBasicMaterial, meshStandardMaterial)
import React.R3F.Three.Objects (group, mesh)
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
  gui <- LilGUI.create
  soft <- mkSoftShadows
  light <- mkLight
  room <- mkRoom
  sphere <- mkSphere

  React.component "Canvas" \_ -> React.do
    pure $ canvas
      { shadows: true
      , camera: { position: [ 5.0, 2.0, 10.0 ], fov: 50.0 }
      , children:
          [ perf { position: "bottom-right" }
          , soft gui
          , cameraControls { makeDefault: true }
          , ambientLight { intensity: 0.4 }
          , light unit
          , room unit
          , sphere { position: [ 0.0, 5.0, -8.0 ], scale: 1.0 }
          , sphere { position: [ 2.0, 4.0, -8.0 ], scale: 0.9 }
          , sphere { position: [ -2.0, 2.0, -8.0 ], scale: 0.8 }
          , sky { inclination: 0.5, scale: 20.0 }
          ]
      }

mkSoftShadows :: Component LilGUI.GUI
mkSoftShadows = React.component "softShadows" \gui -> React.do
  softShadow /\ set <- useState
    { enabled: true
    , size: 35.0
    , focus: 0.5
    , samples: 16
    }

  useEffectOnce do
    LilGUI.add @"enabled" softShadow LilGUI.Checkbox gui
      >>= LilGUI.name "Enable soft shadows"
      >>= LilGUI.onChange (\enabled -> set (_ { enabled = enabled }))
        >>> void
    LilGUI.add @"size" softShadow (LilGUI.NumberField 0.0 100.0 1.0) gui
      >>= LilGUI.name "Soft shadow size"
      >>= LilGUI.onChange (\size -> set (_ { size = size }))
        >>> void
    LilGUI.add @"focus" softShadow (LilGUI.NumberField 0.0 2.0 0.1) gui
      >>= LilGUI.name "Soft shadow focus"
      >>= LilGUI.onChange (\focus -> set (_ { focus = focus }))
        >>> void
    LilGUI.add @"samples" softShadow (LilGUI.NumberField 1.0 40.0 1.0) gui
      >>= LilGUI.name "Soft shadow samples"
      >>= LilGUI.onChange (\samples -> set (_ { samples = samples }))
        >>> void
    pure mempty

  pure $
    if softShadow.enabled then
      softShadows
        { size: softShadow.size
        , focus: softShadow.focus
        , samples: softShadow.samples
        }
    else React.empty

mkLight :: Component Unit
mkLight = React.component "Light" \_ -> React.do
  pure $ directionalLight
    { position: [ 5.0, 5.0, -8.0 ]
    , castShadow: true
    , intensity: 5.0
    , "shadow-mapSize": [ 2048.0, 2048.0 ]
    , "shadow-bias": -0.001
    , children:
        [ orthographicCamera
            -- Use the `Drei.orthographicCamera` binding here will not have the
            -- desired effect. How to prevent misuse like this through, ideally,
            -- the type system? Basically a binding like this one can happen in
            -- three places:
            --    Three.Types
            --    Three.Cameras
            --    Drei.Cameras
            -- They are all useful but should distinguish from each other
            -- somehow.
            { left: -8.5, right: 8.5, top: 8.5, bottom: -8.5, near: 0.1, far: 20.0 }
            { attach: "shadow-camera" }
        ]
    }

mkRoom :: Component Unit
mkRoom = do
  React.component "Room" \_ -> React.do
    { nodes } <- useGLTF "/assets/gltf/room-transformed.glb"
    let
      meshes =
        [ mesh $ enableShadows nodes."Object_2"
        , mesh $ enableShadows nodes."Object_3"
        , mesh $ enableShadows nodes."Object_4"
        , mesh $ enableShadows nodes."Object_6"
        , mesh $ enableShadows nodes."Object_7"
        , mesh $ enableShadows nodes."Object_8"
        , mesh $ enableShadows nodes."Object_9"
        , mesh $ enableShadows nodes."Object_10"
        , mesh $ enableShadows nodes."Object_11"
        , mesh $ enableShadows nodes."Object_12"
        , mesh $ enableShadows nodes."Object_16"
        , mesh $ enableShadows nodes."Object_5"
        , mesh
            { geometry: nodes."Object_13".geometry
            , children:
                [ meshStandardMaterial { transparent: true, opacity: 0.5 } ]
            }
        , mesh $ enableShadows nodes."Object_14"
        , mesh $ enableShadows nodes."Object_15"
        , mesh $ enableShadows nodes."Object_17"
        , mesh $ enableShadows nodes."Object_18"
        ]

    pure $ group
      { scale: 0.5
      , rotation: [ -pi / 2.0, 0.0, 0.0 ]
      , position: [ 0.0, -1.0, 0.0 ]
      , children: meshes
      }
  where
  enableShadows props = props { castShadow = true, receiveShadow = true }

mkSphere :: Component
  { position :: Array Number
  , scale :: Number
  }
mkSphere = React.component "sphere" \props -> React.do
  pure $ float
    { floatIntensity: 15.0
    , children:
        [ mesh
            { castShadow: true
            , position: props.position
            , scale: props.scale
            , children:
                [ sphereGeometry {} {}
                , meshBasicMaterial
                    { color: "hotpink"
                    , roughness: 1.0
                    }
                ]
            }
        ]
    }

