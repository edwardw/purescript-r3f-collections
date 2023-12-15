module LTFE.Helpers.SceneControls where

import Prelude

import Data.Tuple.Nested ((/\))
import React.Basic.Hooks (Component, useEffect, useEffectOnce, useState)
import React.Basic.Hooks as R
import React.R3F.Drei.Controls.LilGUI as LilGUI
import React.R3F.Drei.Loaders (useTexture)
import React.R3F.Hooks (applyProps, useThree)
import React.R3F.Three.Constants (equirectangularReflectionMapping)
import React.R3F.Three.Internal (null)
import React.R3F.Three.Objects (group)
import React.R3F.Three.Types (createColor, createFog, meshNormalMaterial)

initSceneControls
  :: LilGUI.GUI
  -> Boolean
  -> Boolean
  -> Component Unit
initSceneControls gui fogEnabled isOpen = do
  sceneControls <- LilGUI.addFolder "Scene" gui

  R.component "" \_ -> R.do
    props /\ setProps <- useState
      { bgColor: "Gray"
      , fog: { color: "#ffffff", near: 0.0025, far: 50.0 }
      }
    override /\ setOverride <- useState false

    useEffectOnce do
      LilGUI.add @"toggle" { toggle: setOverride not } LilGUI.Button sceneControls
        >>= LilGUI.name "Toggle Override Material" >>> void
      LilGUI.add @"bgColor" props
        (LilGUI.Dropdown [ "White", "Black", "Gray", "Color", "Texture", "Cubemap" ])
        sceneControls
        >>= LilGUI.name "Background"
        >>= LilGUI.onChange (onBgColor setProps)
          >>> void

      when fogEnabled do
        fogControls <- LilGUI.addFolder "Fog" sceneControls
        LilGUI.addColor @"color" props.fog fogControls
          >>= LilGUI.onChange (onFogColor setProps) >>> void
        LilGUI.add @"near" props.fog (LilGUI.NumberField 0.0 10.0 0.1) fogControls
          >>= LilGUI.onChange (onFogNear setProps) >>> void
        LilGUI.add @"far" props.fog (LilGUI.NumberField 0.0 100.0 0.1) fogControls
          >>= LilGUI.onChange (onFogFar setProps) >>> void

      void $ if isOpen then LilGUI.open sceneControls else LilGUI.close sceneControls
      pure mempty

    scene <- useThree @"scene"

    bgTexture <- useTexture "/assets/textures/wood/abstract-antique-backdrop-164005.jpg"
    cubemapTexture <- useTexture "/assets/equi.jpeg"

    useEffect props.bgColor do
      applyProps scene { environment: null }
      case props.bgColor of
        "Color" -> do
          c <- createColor "skyblue"
          applyProps scene { background: c }
        "Texture" -> do
          applyProps scene { background: bgTexture }
        "Cubemap" -> do
          applyProps cubemapTexture { mapping: equirectangularReflectionMapping }
          applyProps scene { background: cubemapTexture, environment: cubemapTexture }
        color -> do
          c <- createColor color
          applyProps scene { background: c }
      pure mempty

    -- `useEffect props.fog` will not do, why?
    useEffect (props.fog.color /\ props.fog.near /\ props.fog.far) do
      fog <- createFog props.fog
      applyProps scene { fog }
      pure mempty

    useEffect override do
      if override then do
        material <- meshNormalMaterial
        applyProps scene { overrideMaterial: material }
      else do
        applyProps scene { overrideMaterial: null }
      pure mempty

    pure $ group {}

  where

  onBgColor set color = set (_ { bgColor = color })
  onFogColor set color = set (_ { fog { color = color } })
  onFogNear set near = set (_ { fog { near = near } })
  onFogFar set far = set (_ { fog { far = far } })

