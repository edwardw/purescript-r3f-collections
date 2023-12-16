module Stories.Performance.ShaderGrass where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Number (pi)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Exception as Exception
import Performace.Grass (mkGrass)
import React.Basic.DOM.Client as Client
import React.Basic.Hooks (Component)
import React.Basic.Hooks as React
import React.Basic.Hooks.Suspense (suspense)
import React.Basic.StrictMode as StrictMode
import React.R3F.Drei.Controls (orbitControls)
import React.R3F.Drei.Misc (perf)
import React.R3F.Drei.Staging (sky)
import React.R3F.Three.Lights (ambientLight)
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
  grass <- mkGrass

  React.component "" \_ -> React.do
    pure $ canvas
      { shadows: "soft"
      , "camera-position": [ 15, 15, 10 ]
      , children:
          [ orbitControls { minPolarAngle: pi / 2.5, maxPolarAngle: pi / 2.5 }
          , perf { position: "bottom-right" }
          , sky { azimuth: 1.0, inclination: 0.6, distance: 1_000 }
          , ambientLight {}
          , suspense
              { fallback: React.empty
              , children:
                  [ grass $ { bW: 0.12, bH: 1.0, joints: 5 } /\ 100.0 /\ 50_000 ] }
          ]
      }

