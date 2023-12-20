module Effect.FrostedGlassOverlay where

import Prelude

import Data.Tuple.Nested (type (/\), (/\))
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component)
import React.Basic.Hooks as React

mkOverlay :: Component Boolean
mkOverlay = do
  list <- mkList

  React.component "overlay" \open -> React.do
    let
      cssContainer = DOM.css
        { position: "absolute"
        , top: "50%"
        , left: "50%"
        , width: "400px"
        , transform: "translate(-50%, -50%)"
        , borderLeft: "1px solid black"
        , paddingLeft: "40px"
        , userSelect: "none"
        }
      cssH1 = DOM.css
        { fontSize: "2em"
        , marginBottom: "0.5em"
        }
      cssH3 = DOM.css
        { fontFamily: "'Inter', sans-serif"
        , fontWeight: 800
        , fontSize: "5em"
        , letterSpacing: "-4px"
        , lineHeight: "1em"
        , margin: 0
        }
      cssH3Hollow = DOM.css
        { fontFamily: "'Inter', sans-serif"
        , fontWeight: 800
        , fontSize: "5em"
        , letterSpacing: "-4px"
        , lineHeight: "1em"
        , margin: 0
        , "WebkitTextStroke": "1px black"
        , color: "transparent"
        }
      prompt = DOM.h1
        { style: cssH1
        , children:
            [ DOM.text $ if open then "NASA" else "hover over me"
            ]
        }
      items =
        [ DOM.h3 { style: cssH3, children: [ DOM.text "James" ] }
        , DOM.h3 { style: cssH3, children: [ DOM.text "Webb" ] }
        , DOM.h3 { style: cssH3Hollow, children: [ DOM.text "Space" ] }
        , DOM.h3 { style: cssH3, children: [ DOM.text "Telescope" ] }
        ]

    pure $ DOM.div
      { style: cssContainer
      , children: [ prompt ] <> [ list $ items /\ open ]
      }

mkList :: Component (Array JSX /\ Boolean)
mkList = React.component "motionList" \(children /\ open) -> React.do
  let
    cssContainer = DOM.css
      { listStyle: "none"
      , opacity: if open then 1 else 0
      , height: if open then "auto" else "0"
      , transition: "all"
      }
    cssItem = DOM.css
      { listStyle: "none"
      , opacity: if open then 1 else 0
      , y: if open then "0" else "100%"
      }

  pure $ DOM.ul
    { style: cssContainer
    , children:
        map
          (\c -> DOM.li { style: cssItem, children: [ c ] })
          children
    }

