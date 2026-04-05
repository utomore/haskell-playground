module Easy where -- 宣告模組名稱

import Data.List (insert) -- 用於簡單插入，或用下面的自定義邏輯
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- 1_TwoSum
twoSum :: [Int] -> Int -> Maybe (Int, Int)
twoSum nums t =
  let indexes = zip [0 ..] nums
      paires = [(i, j) | (i, x) <- indexes, (j, y) <- indexes, i /= j, x + y == t]
   in case paires of
        (p : _) -> Just p
        [] -> Nothing

twoSum' :: [Int] -> Int -> Maybe (Int, Int)
twoSum' [] _ = Nothing
twoSum' nums target = go (zip [0 ..] nums) Map.empty
 where
  go [] _ = Nothing
  go ((i, x) : rest) seen =
    let complement = target - x
     in case Map.lookup complement seen of
          Just j -> Just (j, i)
          Nothing -> go rest (Map.insert x i seen)

-- 283. Move Zeroes

moveZeroes' :: [Int] -> [Int]
moveZeroes' [] = []
moveZeroes' nums =
  let data' = filter (/= 0) nums
      zeroLength = (length nums - length data')
   in data' ++ replicate zeroLength 0

-- 217 Contians Duplicate

containsDuplicate' :: [Int] -> Bool
containsDuplicate' [] = True
containsDuplicate' nums = length nums /= Set.size (Set.fromList nums)
