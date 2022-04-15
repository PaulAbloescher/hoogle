-- {-# OPTIONS -fplugin=Plugin.HolePlugin -fplugin-opt=Plugin.HolePlugin:http://localhost:8000 -funclutter-valid-hole-fits #-}
-- {-# OPTIONS -fmax-valid-hole-fits=10 #-}
{-# LANGUAGE OverloadedStrings #-}
module Plugin.PluginDemo where

import Plugin.HolePlugin ()
import Data.Char (toUpper, isUpper)
import Data.Text

-- test_one :: Text -> Text 
-- test_one x = _remove_leading_whitespace x

-- test_two :: [a] -> [a] 
-- test_two x = _turn_around x

-- autoTyInference :: [a] -> a
-- autoTyInference xs = _get_first_element xs 1
