module LTFE.Helpers.AddRemoveCubeControls where

import Prelude

import Data.Array as A
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe)
import Data.Number (pi)
import Data.Tuple.Nested ((/\))
import Effect.Random (randomInt, randomRange)
import React.Basic (JSX, keyed)
import React.Basic.Hooks (Component, useEffectOnce, useState)
import React.Basic.Hooks as React
import React.R3F.Drei.Controls.LilGUI as LilGUI
import React.R3F.Drei.Shapes (box)
import React.R3F.Three.Materials (meshStandardMaterial)
import React.R3F.Three.Objects (group)

addRemoveCubeControls
  :: LilGUI.GUI
  -> Maybe JSX
  -> Component Unit
addRemoveCubeControls gui material = React.component "cubes" \_ -> React.do
  cubes /\ setCubes <- useState Map.empty

  useEffectOnce do
    LilGUI.add @"add" { add: addCube setCubes } LilGUI.Button gui
      >>= LilGUI.name "Add a cube" >>> void
    LilGUI.add @"remove" { remove: removeCube setCubes } LilGUI.Button gui
      >>= LilGUI.name "Remove a cube" >>> void
    pure mempty

  pure $ group { children: A.fromFoldable $ Map.values cubes }

  where

  addCube setCubes = do
    x <- randomRange (-4.0) 4.0
    y <- randomRange (-3.0) 3.0
    z <- randomRange (-4.0) 4.0
    xr <- randomRange 0.0 (pi * 2.0)
    yr <- randomRange 0.0 (pi * 2.0)
    zr <- randomRange 0.0 (pi * 2.0)
    r <- randomInt 0 255
    g <- randomInt 0 255
    b <- randomInt 0 255

    setCubes \cubes ->
      let
        name = "cube-" <> show (Map.size cubes)
        color = "rgb(" <> show r <> "," <> show g <> "," <> show b <> ")"
        mat = fromMaybe
          (meshStandardMaterial { attach: "material", args: [ { color, roughness: 0.1, metalness: 0.9 } ] })
          material
        cube = keyed name $ box
          { width: 0.5, height: 0.5, depth: 0.5 }
          { castShadow: true
          , position: [ x, y, z ]
          , rotation: [ xr, yr, zr ]
          , children: [ mat ]
          }
      in
        Map.insert name cube cubes

  removeCube setCubes = setCubes \cubes ->
    let
      last = Map.size cubes - 1
    in
      if (last < 0) then cubes
      else Map.delete ("cube-" <> show last) cubes

