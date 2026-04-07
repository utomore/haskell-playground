module Main where

-- 從 Library 引入
import Easy (moveZeroes', twoSum', triangleNumber', triangleNumber)
import Test.QuickCheck

-- 定義一個測試屬性
prop_twoSum_basic :: Bool
prop_twoSum_basic = twoSum' [2, 7, 11] 9 == Just (0, 1)

-- 定義一個「屬性 (Property)」
-- 這個屬性的意思是：如果 twoSum' 找到了索引 (i, j)，
-- 那麼 nums[i] + nums[j] 必須等於 target
prop_twoSum_is_correct :: [Int] -> Int -> Bool
prop_twoSum_is_correct nums target =
  case twoSum' nums target of
    Nothing -> True -- 沒找到不算錯，因為不是每組數據都有解
    Just (i, j) ->
      -- 驗證索引是否合法，且數值相加是否等於 target
      let val1 = nums !! i
          val2 = nums !! j
       in val1 + val2 == target

-- 這是你的測試屬性
prop_twoSum_RandomPlacement :: Property
prop_twoSum_RandomPlacement =
  -- 1. 隨機生成兩個要當作答案的數字，以及一串雜訊數字
  forAll (arbitrary :: Gen (Int, Int, [Int])) $ \(val1, val2, others) ->
    -- 2. 隨機選擇兩個插入位置 (0 到 others 的長度之間)
    forAll (choose (0, length others)) $ \pos1 ->
      forAll (choose (0, length others)) $ \pos2 ->
        let
          target = val1 + val2
          -- 先插入第一個數
          (front, back) = splitAt pos1 others
          listWithOne = front ++ [val1] ++ back
          -- 再插入第二個數 (這確保了兩個數都被塞進去了，且位置隨機)
          (front2, back2) = splitAt pos2 listWithOne
          nums = front2 ++ [val2] ++ back2
         in
          -- 3. 執行測試
          case twoSum' nums target of
            Just (i, j) -> (nums !! i) + (nums !! j) == target
            Nothing -> False -- 因為我們保證有解，所以拿到 Nothing 就是 Bug！

-- 專門測試重複數字的情況
prop_twoSum_Duplicate :: Int -> [Int] -> Property
prop_twoSum_Duplicate val others =
  let target = val * 2
      nums = val : val : others -- 這裡可以手動洗牌或隨機插入
   in property $ case twoSum' nums target of
        Just (i, j) -> i /= j && (nums !! i) + (nums !! j) == target
        Nothing -> False

-- 283.
frequentZeros :: Gen Int
frequentZeros =
  frequency
    [ (1, return 0) -- 50% 的機率產生 0
    , (1, arbitrary) -- 50% 的機率產生隨機整數
    ]

-- 1. 常規案例
prop_moveZeroes_basic :: Bool
prop_moveZeroes_basic = moveZeroes' [0, 1, 0, 3, 12] == [1, 3, 12, 0, 0]

-- 2. 全是 0
prop_moveZeroes_allZeros :: Bool
prop_moveZeroes_allZeros = moveZeroes' [0, 0, 0] == [0, 0, 0]

-- 3. 沒有 0
prop_moveZeroes_noZeros :: Bool
prop_moveZeroes_noZeros = moveZeroes' [1, 2, 3] == [1, 2, 3]

-- 4. 空陣列
prop_moveZeroes_empty :: Bool
prop_moveZeroes_empty = moveZeroes' [] == []

prop_length_stays_the_same :: [Int] -> Bool
prop_length_stays_the_same nums = length (moveZeroes' nums) == length nums

prop_order_preserved :: [Int] -> Bool
prop_order_preserved nums =
  filter (/= 0) (moveZeroes' nums) == filter (/= 0) nums

-- 使用 forAll 來強制使用這個產生器
prop_zeros_at_end_smart :: Property
prop_zeros_at_end_smart = forAll (listOf frequentZeros) $ \nums ->
  let res = moveZeroes' nums
      zeroCount = length (filter (== 0) nums)
      tailPart = drop (length nums - zeroCount) res
   in all (== 0) tailPart

-- 611
prop_valid_triangle_number :: Property
prop_valid_triangle_number = 
    -- 使用 forAll 配合 choose 指定長度，這樣就不會發生 discarded 了
    forAll (choose (0, 40)) $ \n ->
        forAll (vectorOf n (choose (0, 100))) $ \nums ->
            triangleNumber nums === triangleNumber' nums

-- 如果想跑更專業的測試，可以加上 label 觀察分佈
prop_with_label :: [Int] -> Property
prop_with_label nums = 
    label (sizeCategory (length nums)) $ 
    length nums < 40 ==> triangleNumber nums === triangleNumber' nums
  where
    sizeCategory n
        | n == 0    = "empty"
        | n < 3     = "too small"
        | otherwise = "valid size"




main :: IO ()
main = do
  putStrLn "Running tests..."
  quickCheck prop_twoSum_basic
  quickCheck prop_twoSum_is_correct
  quickCheck prop_twoSum_RandomPlacement
  quickCheck prop_twoSum_Duplicate

  quickCheck prop_moveZeroes_basic
  quickCheck prop_moveZeroes_allZeros
  quickCheck prop_moveZeroes_noZeros
  quickCheck prop_moveZeroes_empty
  quickCheck prop_length_stays_the_same
  quickCheck prop_order_preserved
  quickCheck prop_zeros_at_end_smart


  putStrLn "Checking triangleNumber vs triangleNumber'..."
  quickCheck prop_valid_triangle_number
  putStrLn "End tests..."
