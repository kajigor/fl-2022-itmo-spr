1.
Вначале приведу словестное описание.

Начальный символ `S`. Терминалы -- латинские буквы, цифры,
двоеточие, двойные кавычки, запятая, фигурные и квадратные скобки

В грамматике существует набор нетерминальных символов `*Continuation`,
которые используюся для описания потенциально бесконечных структур
(e.g. списков) типа `*`.

Стартовый символ раскрывается в пару фигурных скобок и список пар
(Строка, Определение автомата). Определение автомата это либо автомат,
либо унарная операция с автоматом, либо бинарная операция с двумя автоматами.
Автоматы в операциях могут задоваться явно или строкой (их именем).

Автомат это набор пар (Ключ, Значение).
Множество ключей фиксируется грамматикой.

```
S -> { AutomataFieldContinuation }

AutomataFieldContinuation -> AutomataField, AutomataFieldContinuation | AutomataField

AutomataField -> String: AutomataDef

AutomataDef -> Automata | [AutomataOrSting, BiOperation, AutomataOrString ] | [ "star", AutomataOrString ]

AutomataOrString -> Automata | String

BiOperation -> "union" | "concat" | "diff" | "intersect"

Automata -> {
    "alphabet": ListOfStringOrNum,
    "states": ListOfString,
    "terminals": ListOfString,
    "start": String,
    "sigma": SigmaList
}

SigmaList = [ SigmaObjectContinuation ]

ListOfStringOrNum -> [ StringOrNumContinuation ]

ListOfString -> [ StringContinuation ]

SigmaObjectContinuation -> SigmaObject, SigmaObjectContinuation | SigmaObject

SigmaObject -> {
    "from": String,
    "to": String,
    "by": StringOrNum
}

StringOrNumContinuation -> StringOrNum, StringOrNumContinuation | StringOrNum

StringContinuation = String, StringContinuation | String

StringOrNum -> String | Num

String -> Regex("[a-zA-Z0-9]+")

Num -> Regex([1-9][0-9]+)
```
