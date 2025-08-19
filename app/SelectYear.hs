module SelectYear (selectYear) where

import Util
import qualified Year2023 as Y23
import qualified Year2024 as Y24

selectYear :: Int -> Int -> Int -> Solution
selectYear year = case year of
    2023 -> Y23.selectDay
    2024 -> Y24.selectDay
    _ -> const $ const $ const $ Error "Unknown year."
