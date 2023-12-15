module Stories.LTFE.BasicScene where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Exception as Exception
import LTFE.Helpers.AddRemoveCubeControls (addRemoveCubeControls)
import LTFE.Helpers.Bootstrap (bootstrap)
import LTFE.Helpers.Floor (floatingFloor)
import LTFE.Helpers.Helpers (initHelperControls)
import LTFE.Helpers.SceneConfig (defaultSceneOptions, defaultSceneProps)
import LTFE.Helpers.SceneControls (initSceneControls)
import React.Basic.DOM.Client as Client
import React.Basic.Hooks (Component, useEffectOnce, useState)
import React.Basic.Hooks as React
import React.Basic.StrictMode as StrictMode
import React.R3F.Drei.Controls.LilGUI as LilGUI
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
  floor <- floatingFloor 10.0
  helpers <- initHelperControls gui
  sceneControls <- initSceneControls gui true true
  addRemove <- addRemoveCubeControls gui Nothing
  container <- bootstrap defaultSceneOptions

  React.component "App" \_ -> React.do
    sceneState <- useState defaultSceneProps

    useEffectOnce do
      let _ /\ setScene = sceneState
      setScene (_ { cameraPosition = [ -7.0, 2.0, 5.0 ] })
      pure mempty

    pure $ canvas
      { shadows: "soft"
      , children:
          [ container sceneState
          , floor unit
          , helpers unit
          , sceneControls unit
          , addRemove unit
          ]
      }

