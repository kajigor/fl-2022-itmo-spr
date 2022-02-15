2. Нельзя, потому что парсеры разбирают строку слева направо, а требуемый список должен содержать выражения справа налево

3.
    * Состояния: {`LessThanOne`, `One`, `GreaterThanOne`}, начальное -- `LessThanOne`
    ```haskell
    data State = Less | Eq | Greater
    
    auto :: State -> String -> Bool
    auto Eq [] = False
    auto _ [] = True
    auto Greater _ = True
    auto Less (c : s) | c == 'c'  = auto Eq s
                      | otherwise = auto Less s
    auto Eq (c : s) | c == 'c'  = auto Greater s
                    | otherwise = auto Eq s

    input = "..."
    result = auto Less input
    ```
    * Состояние -- найденный префикс искомой подстроки, начальное -- пустая строка
    ```haskell
    auto :: String -> String -> Bool
    auto "abbab" _ = True
    auto _ [] = False
    auto "" (c : s) | c == 'a' = auto "a" s 
                    | c == 'b' = auto "" s
    auto "a" (c : s) | c == 'a' = auto "a" s
                     | c == 'b' = auto "ab" s
    auto "ab" (c : s) | c == 'a' = auto "a" s
                      | c == 'b' = auto "abb" s
    auto "abb" (c : s) | c == 'a' = auto "abba" s
                       | c == 'b' = auto "" s
    auto "abba" (c : s) | c == 'a' = auto "a" s
                        | c == 'b' = auto "abbab" s

    input = "..."
    result = auto "" input
    ```