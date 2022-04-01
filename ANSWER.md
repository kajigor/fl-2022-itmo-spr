1. Нет

   Первый язык разбирается таким автоматом
   ```mermaid
   flowchart LR
       1 -- a --> 2[[2]]
       1 -- b --> 1
       2 -- a --> 3
       2 -- b --> 3
       3 -- a --> 2[[2]]
       3 -- b --> 3
   ```
   Второй таким
   ```mermaid
   flowchart LR
       1 -- a --> 2[[2]]
       1 -- b --> 1
       2 -- a --> 1
       2 -- b --> 1
   ```
2. 
3.
Нетерминалы `Mult`, `Expr`, `Num`. Терминалы -- цифры
```
Expr -> Mult | (Expr) | Mult + Expr | Expr + Mult
Mult -> Num  | (Mult) | Num * Mult  | Mult * Num
Num  -> 0    | [1-9][0-9]*
```