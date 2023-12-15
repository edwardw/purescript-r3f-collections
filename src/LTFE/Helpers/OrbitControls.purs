module LTFE.Helpers.OrbitControls where

import Prelude

import Data.Number (pi)
import React.Basic.Hooks (Component)
import React.Basic.Hooks as React
import React.R3F.Drei.Controls (orbitControls)

initOrbitControls :: Component Unit
initOrbitControls = React.component "orbitControls" \_ -> React.do
  pure $ orbitControls
    { enableDamping: true
    , dampingFactor: 0.05
    , minDistance: 1.0
    , maxDistance: 100.0
    , minPolarAngle: pi / 4.0
    , maxPolarAngle: 3.0 * pi / 4.0
    }

