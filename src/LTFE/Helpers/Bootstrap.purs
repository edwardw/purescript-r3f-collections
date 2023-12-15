module LTFE.Helpers.Bootstrap where

import Prelude

import Data.Int (toNumber)
import Data.Tuple.Nested ((/\))
import LTFE.Helpers.Lighting (initLighting)
import LTFE.Helpers.OrbitControls (initOrbitControls)
import LTFE.Helpers.SceneConfig (SceneOptions, UseStateType, SceneProps)
import React.Basic.Hooks (Component, useEffectOnce)
import React.Basic.Hooks as React
import React.R3F.Drei.Cameras (perspectiveCamera)
import React.R3F.Drei.Misc (perf)
import React.R3F.Hooks (applyProps, useThree)
import React.R3F.Three.Constants (vSMShadowMap)
import React.R3F.Three.Objects (group)
import Web.HTML as HTML
import Web.HTML.Window as Window

bootstrap
  :: SceneOptions
  -> Component (UseStateType SceneProps)
bootstrap sceneOpts = do
  camera <- mkCamera
  orbitControls <- initOrbitControls
  lightings <- initLighting

  React.component "container" \(sceneProps /\ _) -> React.do
    gl <- useThree @"gl"
    useEffectOnce do
      applyProps gl { "shadowMap-type": vSMShadowMap }
      pure mempty

    pure $ group
      { children:
          [ camera sceneProps
          , orbitControls unit
          , lightings sceneOpts.disableShadows
          , perf { position: "bottom-right" }
          ]
      }

mkCamera :: Component SceneProps
mkCamera = do
  window <- HTML.window
  innerWidth <- Window.innerWidth window
  innerHeight <- Window.innerHeight window

  React.component "camera" \props -> React.do
    pure $ perspectiveCamera
      { makeDefault: true
      , fov: 75.0
      , aspect: toNumber (innerWidth) / toNumber (innerHeight)
      , position: props.cameraPosition
      , near: 0.1
      , far: 1000.0
      }

