module Years.Year2020.Day21 (part1, part2) where

import Util.Util
import Data.Bifunctor
import Data.Tuple.Extra
import Data.List.Extra
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Allergen = String
type Ingredient = String
type Food = ([Ingredient], [Allergen])

getResult :: [String] -> ([Ingredient], Map Allergen (Set Ingredient))
getResult input = (concatMap fst foods, foldr refinePossibleIngredients initPossible foods)
    where foods = map (bimap words (splitOn ", ") . pair . splitOn "(contains " . ini) input
          (ingredients, allergens) = both (Set.fromList . concat) $ unzip foods
          initPossible = Map.fromSet (const ingredients) allergens
          refinePossibleIngredients :: Food -> Map Allergen (Set Ingredient) -> Map Allergen (Set Ingredient)
          refinePossibleIngredients (is, as) mappings = foldr (Map.adjust (Set.intersection $ Set.fromList is)) mappings as

part1 :: Solution
part1 input = let (ingredients, possible) = getResult input
                  counts = Map.fromListWith (+) $ map (, 1) ingredients
                  withAllergens = Set.unions possible
              in V $ sum $ Map.withoutKeys counts withAllergens


determineMappings :: Map Allergen (Set Ingredient) -> Map Allergen Ingredient
determineMappings possible | null possible = Map.empty
                           | null single = error "No single mappings found."
                           | otherwise = Map.union single $ determineMappings $ Map.map (`Set.difference` singleIngredients) $ Map.difference possible single
    where single = Map.map Set.findMin $ Map.filter ((== 1) . Set.size) possible
          singleIngredients = Set.fromList $ Map.elems single

part2 :: Solution
part2 input = let (_, mappings) = getResult input
                  single = determineMappings mappings
              in Msg $ intercalate "," $ Map.elems single
