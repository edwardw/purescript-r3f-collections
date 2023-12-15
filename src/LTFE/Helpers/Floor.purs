module LTFE.Helpers.Floor where

import Prelude hiding (add)

import Data.Number (pi)
import React.Basic.Hooks (Component)
import React.Basic.Hooks as React
import React.R3F.Drei.Shapes (box, plane)
import React.R3F.Three.Materials (meshLambertMaterial, meshStandardMaterial)
import React.R3F.Three.Types (createColor)

foreverPlane :: Component Unit
foreverPlane = do
  white <- createColor "#ffffff"

  React.component "foreverPlane" \_ -> React.do
    pure $ plane
      { width: 10_000.0, height: 10_000.0 }
      { name: "forever-floor"
      , position: [ 0.0, -2.0, 0.0 ]
      , rotation: [ pi / -2.0, 0.0, 0.0 ]
      , receiveShadow: true
      , children:
          [ meshLambertMaterial { color: white } { attach: "material" } ]
      }

floatingFloor :: Number -> Component Unit
floatingFloor size = do
  React.component "floatingFloor" \_ -> React.do
    pure $ box
      { width: size, height: 0.25, depth: size }
      { name: "floating-floor"
      , position: [ 0.0, -2.0, -1.0 ]
      , receiveShadow: true
      , children:
          [ meshStandardMaterial { attach: "material", args: [ { color: "#dddddd" } ] } ]
      }

