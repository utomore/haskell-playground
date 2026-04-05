Original directory name: 1_Mini_Exp_Eva

# Mini-Expression-Evaluator Spec

## Interactive
```
>>> x = 3
x = 3

>>> y = x + 1
y = 4

>>> y ^ 2 + PI
20.869

>>> quit
```

## Support Calculator
- 四則運算: `+`, `-`, `*`, `/`
- 次方: `^`
- 括號: `(` `)`
- 負數: `-3`, `-(x+1)`
- 常見常數: `PI`, `E`

## Variables
- 賦予數值: `x = <expr>`
- 可使用跨行引用之前定義的變數
- 變數值是計算後的結果(不是 lazy?, 不是 symbolic?)

## 錯誤處理
- Parser Error: `(1 +` -> 告知表達式不完整
- Runtime Error: `1/0` -> 告知無法計算
- 未定義變數: `x+1` (x無定義) -> 告知變數不存在
