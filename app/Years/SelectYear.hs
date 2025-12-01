module Years.SelectYear (selectYear) where

import Util.Util
import qualified Years.Year2017 as Y17
import qualified Years.Year2018 as Y18
import qualified Years.Year2019 as Y19
import qualified Years.Year2020 as Y20
import qualified Years.Year2021 as Y21
import qualified Years.Year2022 as Y22
import qualified Years.Year2023 as Y23
import qualified Years.Year2024 as Y24
import qualified Years.Year2025 as Y25

selectYear :: Int -> Int -> Int -> Solution
selectYear year = case year of
    2017 -> Y17.selectDay
    2018 -> Y18.selectDay
    2019 -> Y19.selectDay
    2020 -> Y20.selectDay
    2021 -> Y21.selectDay
    2022 -> Y22.selectDay
    2023 -> Y23.selectDay
    2024 -> Y24.selectDay
    2025 -> Y25.selectDay
    _ -> const $ const $ const $ Error "Unknown year."
