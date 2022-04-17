{-# OPTIONS -fplugin=Plugin.HolePlugin -fplugin-opt=Plugin.HolePlugin:http://localhost:8000 -funclutter-valid-hole-fits #-}
{-# OPTIONS -fmax-valid-hole-fits=10 #-}
{-# LANGUAGE OverloadedStrings #-}
module Plugin.PluginDemo where

import qualified Data.Text as T

simpleTitleCase :: T.Text -> T.Text 
simpleTitleCase = T.unwords . map _capitalize_first_char . T.words

-- Console Output Without Ranking:
-- "Host: http://localhost:8000"
-- "Type: :: Text -> Text"
-- "Query: "
--         Only Hoogle: tail :: HasCallStack => Text -> Text
--         Only Hoogle: init :: HasCallStack => Text -> Text
--         Only Hoogle: reverse :: Text -> Text
--         Only Hoogle: toCaseFold :: Text -> Text
--         Only Hoogle: toLower :: Text -> Text
--         Only Hoogle: toUpper :: Text -> Text
--         Only Hoogle: toTitle :: Text -> Text
--         Only Hoogle: strip :: Text -> Text
--         Only Hoogle: stripStart :: Text -> Text
--         Only Hoogle: stripEnd :: Text -> Text
--         (Some hole fits suppressed; use -fmax-valid-hole-fits=N or -fno-max-valid-hole-fits)

-- Console Output With Ranking:
-- "Host: http://localhost:8000"
-- "Type: :: Text -> Text"
-- "Query: capitalize first char"
--       Valid hole fits include
--         Ranked Hoogle: ucFirst :: Text -> Text
--         Ranked Hoogle: lcFirst :: Text -> Text
--         Ranked Hoogle: camelCaseToHyphenated :: Text -> Text
--         Ranked Hoogle: lowerSymbol :: Text -> Text
--         Ranked Hoogle: toTitle :: Text -> Text
--         Ranked Hoogle: underscoresToCamelCase :: Text -> Text
--         Ranked Hoogle: toUpper :: Text -> Text
--         Ranked Hoogle: toLower :: Text -> Text
--         Ranked Hoogle: stripStart :: Text -> Text
--         Ranked Hoogle: trimMath :: Text -> Text
--         (Some hole fits suppressed; use -fmax-valid-hole-fits=N or -fno-max-valid-hole-fits)


-- stripNumbers :: String -> String 
-- stripNumbers s = filter (not . _numeric_value) s

-- autoTyInference :: [a] -> a
-- autoTyInference xs = _get_first_element xs 1

-- import Data.Char (toUpper, isUpper, isNumber)
