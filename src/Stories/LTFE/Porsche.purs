module Stories.LTFE.Porsche where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Exception as Exception
import LTFE.Helpers.Bootstrap (bootstrap)
import LTFE.Helpers.Floor (floatingFloor)
import LTFE.Helpers.Helpers (initHelperControls)
import LTFE.Helpers.SceneConfig (UseStateType, SceneProps, defaultSceneOptions, defaultSceneProps)
import React.Basic.DOM.Client as Client
import React.Basic.Hooks (Component, useEffectOnce, useState)
import React.Basic.Hooks as R
import React.Basic.Hooks.Suspense (suspense)
import React.Basic.StrictMode as StrictMode
import React.R3F.Drei.Controls.LilGUI as LilGUI
import React.R3F.Drei.Loaders (useEnvironment, useGLTF)
import React.R3F.Hooks (applyProps, useThree)
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
  container <- bootstrap defaultSceneOptions
  floor <- floatingFloor 10.0
  helpers <- initHelperControls gui
  porsche <- mkPorsche

  R.component "Canvas" \_ -> R.do
    sceneState <- useState defaultSceneProps
    pure $ canvas
      { shadows: "soft"
      , children:
          [ container sceneState
          , floor unit
          , helpers unit
          , porsche sceneState
          ]
      }

mkPorsche :: Component (UseStateType SceneProps)
mkPorsche = do
  porsche <- loadPorsche
  R.component "Porsche" \sceneState -> R.do
    env <- useEnvironment "/assets/exr/footprint_court_2k.exr"
    scene <- useThree @"scene"

    useEffectOnce do
      applyProps scene { environment: env }

      let _ /\ setScene = sceneState
      setScene (_ { cameraPosition = [ 7.0, 1.0, -4.0 ] })
      pure mempty

    pure $ group
      { children:
          [ suspense { fallback: R.empty, children: [ porsche unit ] } ]
      }

loadPorsche :: Component Unit
loadPorsche = R.component "porsche" \_ -> R.do
  { nodes } <- useGLTF "/assets/gltf/porsche/porsche-transformed.glb"
  let
    meshes =
      [ mesh nodes."Object_32"
      , mesh nodes."Object_113"
      , mesh nodes."Object_28"
      , mesh nodes."Object_128"
      , mesh nodes."Object_130"
      , mesh nodes."Object_26"
      , mesh nodes."Object_136"
      , mesh nodes."Object_105"
      , mesh nodes."Object_95"
      , mesh nodes."Object_11"
      , mesh nodes."Object_12"
      , mesh nodes."Object_80"
      , mesh nodes."Object_122"
      , mesh nodes."Object_140"
      ]

  pure $ group
    { position: [ 0.0, -1.9, -0.8 ]
    , scale: [ 1.5, 1.5, 1.5 ]
    , children: meshes
    }

