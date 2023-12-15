module LTFE.Helpers.Lighting where

import Prelude

import React.Basic.Hooks (Component)
import React.Basic.Hooks as React
import React.R3F.Three.Lights (ambientLight, directionalLight)
import React.R3F.Three.Objects (group)

initLighting :: Component Boolean
initLighting = React.component "lights" \disableShadow -> React.do
  let
    dirLight = directionalLight
      { color: "#aaaaaa"
      , position: [ 5.0, 12.0, 8.0 ]
      , castShadow: not disableShadow
      , intensity: 1.0
      , "shadow-mapSize": [ 2048.0, 2048.0 ]
      , "shadow-radius": 4.0
      , "shadow-bias": -0.00005
      , "shadow-camera-near": 0.1
      , "shadow-camera-far": 200.0
      , "shadow-camera-right": 10.0
      , "shadow-camera-left": -10.0
      , "shadow-camera-top": 10.0
      , "shadow-camera-bottom": -10.0
      }

  pure $ group
    { children: [ ambientLight { color: "#666666" }, dirLight ] }

