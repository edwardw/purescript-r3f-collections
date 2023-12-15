module LTFE.Helpers.SceneConfig where

import Prelude

import Data.Tuple.Nested (type (/\))
import Effect (Effect)

type SceneOptions =
  { disableShadows :: Boolean
  , disableLights :: Boolean
  , disableDefaultControls :: Boolean
  }

defaultSceneOptions :: SceneOptions
defaultSceneOptions =
  { disableShadows: false
  , disableLights: false
  , disableDefaultControls: false
  }

type SceneProps =
  { cameraPosition :: Array Number
  }

defaultSceneProps :: SceneProps
defaultSceneProps =
  { cameraPosition: [ 0.0, 0.0, 0.0 ]
  }

type UseStateType state = state /\ ((state -> state) -> Effect Unit)

