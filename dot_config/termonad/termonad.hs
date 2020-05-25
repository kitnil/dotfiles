{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Singletons (Sing, sing)
import Termonad.App (defaultMain)
import Termonad.Config
  ( FontConfig, FontSize(FontSizePoints), Option(Set)
  , ShowScrollbar(ShowScrollbarAlways), defaultConfigOptions, defaultFontConfig
  , defaultTMConfig, fontConfig, fontFamily, fontSize, options, showScrollbar
  )
import Termonad.Config.Colour
  ( AlphaColour, ColourConfig, addColourExtension, createColour
  , createColourExtension, cursorBgColour, defaultColourConfig, Palette(ExtendedPalette), foregroundColour, palette
  )
import Termonad.Config.Vec
  ( N4, N8, Vec((:*), EmptyVec), fin_, setAtVec, unsafeFromListVec_
  )

-- | This sets the color of the cursor in the terminal.
--
-- This uses the "Data.Colour" module to define a dark-red color.
-- There are many default colors defined in "Data.Colour.Names".
cursBgColour :: AlphaColour Double
cursBgColour = createColour 204 0 0

-- -- | This sets the colors used for the terminal.  We only specify the background
-- -- color of the cursor.
-- colConf :: ColourConfig (AlphaColour Double)
-- colConf =
--   defaultColourConfig
--     { cursorBgColour = Set cursBgColour
--     }

myColourConfig =
  defaultColourConfig
    -- Set the cursor background colour.  This is the normal colour of the
    -- cursor.
    { cursorBgColour = Set (createColour 218 112 214) -- orchid
    -- Set the default foreground colour of text of the terminal.
    , foregroundColour = Set (createColour 255 255 255) -- white
    -- Set the extended palette that has 8 colours standard colors and then 8
    -- light colors.
    , palette = ExtendedPalette myStandardColours myLightColours
    }
  where
    -- This is a an example of creating a length-indexed linked-list of colours,
    -- using 'Vec' constructors.
    myStandardColours :: Vec N8 (AlphaColour Double)
    myStandardColours =
         createColour 0 0 0 -- dark brown (used as background colour)
      :* createColour 178 24 24
      :* createColour 102 205 0
      :* createColour 178 104 24
      :* createColour 65 105 225
      :* createColour 178 24 178
      :* createColour 24 178 178
      :* createColour 178 178 178
      :* EmptyVec

    -- This is an example of creating a length-indexed linked-list of colours,
    -- using the 'unsafeFromListVec_' function.  'unsafeFromListVec_' is okay to
    -- use as long as you're absolutely sure you have 8 elements.
    myLightColours :: Vec N8 (AlphaColour Double)
    myLightColours =
      unsafeFromListVec_
      [ createColour 104 104 104
      , createColour 255 84 84
      , createColour 84 255 84
      , createColour 238 201 0
      , createColour 84 84 255
      , createColour 255 84 255
      , createColour 84 255 255
      , createColour 255 255 255
      ]

    -- This is an example of updating just a single value in a 'Colour' 'Vec'.
    -- Here we are updating the 5th 'Colour' (which is at index 4).
    _updateSingleColor :: Vec N8 (AlphaColour Double)
    _updateSingleColor =
      let fin4 = fin_ (sing :: Sing N4)
      in setAtVec fin4 (createColour 40 30 150) myStandardColours

-- | This defines the font for the terminal.
fontConf :: FontConfig
fontConf =
  defaultFontConfig
    { fontFamily = "DejaVu Sans Mono"
    , fontSize = FontSizePoints 13
    }

main :: IO ()
main = do
  colExt <- createColourExtension myColourConfig
  let termonadConf =
        defaultTMConfig
          { options =
              defaultConfigOptions
                { fontConfig = fontConf
                  -- Make sure the scrollbar is always visible.
                , showScrollbar = ShowScrollbarAlways
                }
          }
        `addColourExtension` colExt
  defaultMain termonadConf
