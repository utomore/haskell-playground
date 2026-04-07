module Easy where -- 宣告模組名稱

import qualified Data.List as List -- 用於簡單插入，或用下面的自定義邏輯
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as V

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



-- 611. valide Triangle Number
-- O(n^3)
triangleNumber :: [Int] -> Int
triangleNumber nums = length [ (a, b, c) | 
    (c:bs)  <- List.tails sortedRev,
    (b:as)  <- List.tails bs,
    a       <- as,
    a + b > c ]
  where
    sortedRev = reverse $ List.sort $ filter (>0) nums


-- O(n^2)
triangleNumber':: [Int] -> Int
triangleNumber' nums = solve sorted (len - 1)
    where
        sorted = V.fromList $ List.sort (filter (>0) nums)
        len = V.length sorted

        solve _ k | k < 2 = 0
        solve v k = countValid v 0 (k-1) k + solve v (k-1)

        countValid v i j k
            | i >= j = 0
            | (v V.! i) + (v V.! j) > (v V.! k) = (j-i) + countValid v i (j-1) k
            | otherwise = countValid v (i+1) j k

triangleNumber'':: [Int] -> Int
triangleNumber'' nums = sum [ countValid sorted 0 (k-1) k | k <- [ 2 .. len-1 ]]
    where
        sorted = V.fromList $ List.sort (filter (>0) nums)
        len = V.length sorted

        countValid v i j k
            | i >= j = 0
            | (v V.! i) + (v V.! j) > (v V.! k) = (j-i) + countValid v i (j-1) k
            | otherwise = countValid v (i+1) j k


-- 167. Two Sum 2 
twoSum2' :: [Int] -> Int -> Maybe (Int, Int))
twoSum2' nums target = go vnums 0 (len-1)
    where
        vnums = V.fromList nums
        len = V.length vnums
        go v i j 
            | i >= j = Nothing
            | currentSum > target   = go v i (j-1)
            | currentSum < target   = go v (i+1) j
            | otherwise             = Just (i+1, j+1) -- 順應從 [0 .. len) ->  [1..len-1]
            where
                currentSum = (v V.! i) + (v V.! j)
