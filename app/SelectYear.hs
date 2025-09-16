module SelectYear (selectYear) where

import Util
import qualified Year2021 as Y21
import qualified Year2022 as Y22
import qualified Year2023 as Y23
import qualified Year2024 as Y24
import qualified Year2020 as Y20
import qualified Year2019 as Y19

selectYear :: Int -> Int -> Int -> Solution
selectYear year = case year of
    2021 -> Y21.selectDay
    2022 -> Y22.selectDay
    2023 -> Y23.selectDay
    2024 -> Y24.selectDay
    2020 -> Y20.selectDay
    2019 -> Y19.selectDay
    _ -> const $ const $ const $ Error "Unknown year."
