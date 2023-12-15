module LTFE.Helpers.Helpers where

import Prelude

import Data.Array as A
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import React.Basic.Hooks (Component, useEffectOnce, useState)
import React.Basic.Hooks as R
import React.R3F.Drei.Controls.LilGUI as LilGUI
import React.R3F.Three.Helpers (axesHelper, gridHelper, polarGridHelper)
import React.R3F.Three.Objects (group)

initHelperControls
  :: LilGUI.GUI
  -> Component Unit
initHelperControls gui = do
  hControl <- LilGUI.addFolder "Helpers" gui

  R.component "" \_ -> R.do
    children /\ setChildren <- useState Map.empty

    useEffectOnce do
      LilGUI.add @"toggle" { toggle: setChildren (go "axes" axes) } LilGUI.Button hControl
        >>= LilGUI.name "Toggle Axes" >>> void
      LilGUI.add @"toggle" { toggle: setChildren (go "grid" grid) } LilGUI.Button hControl
        >>= LilGUI.name "Toggle Grid" >>> void
      LilGUI.add @"toggle" { toggle: setChildren (go "polar" polar) } LilGUI.Button hControl
        >>= LilGUI.name "Toggle PolarGrid" >>> void
      pure mempty

    pure $ group { children: A.fromFoldable $ Map.values children }

  where

  axes = axesHelper 5.0
  grid = gridHelper { size: 10.0, divisions: 10.0 } {}
  polar = polarGridHelper { radius: 10.0, sectors: 16.0 } {}

  go name helper children =
    case Map.lookup name children of
      Nothing -> Map.insert name helper children
      Just _ -> Map.delete name children

