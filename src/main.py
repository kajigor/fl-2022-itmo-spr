from earley.earley3 import *
import io
import contextlib


def main():
    characters = 'gsitdepA0ab'
    C = Rule("C", Production(""))
    for c in characters:
        C.add(Production(c))
        C.add(Production(c, C))

    B = Rule("B", Production("true"), Production("false"))

    O = Rule("O", Production("union"))

    LC = Rule("LC", Production("\"", C, "\""))
    LC.add(Production("\"", C, "\"", LC))

    LL = Rule("LL", Production("[\"", C, "\",\"", C, "\"]"), Production(""))
    LL.add(Production("[\"", C, "\",\"", C, "\"]", LL))

    EO = Rule("EO", Production("\"", C, "\":[", LL, "]"))
    EO.add(Production("\"", C, "\":[", LL, "],", EO))

    EE = Rule("EE", Production("\"", C, "\":[", LC, "]"))
    EE.add(Production("\"", C, "\":[", LC, "],", EE))

    A = Rule("A", Production("\"g\":[", LC, "],",
                             "\"s\":[", LC, "],",
                             "\"i\":\"", C, "\",",
                             "\"t\":[", LC, "],",
                             "\"d\":", B, ",",
                             "\"e\":{", EO, "},",
                             "\"p\":{", EE, "}"))

    LAO = Rule("LAO", Production(A), Production("\"", C, "\""))
    LAO.add(Production(A, ",\"", O, "\",", LAO))
    LAO.add(Production("\"", C, "\",\"", O, "\",", LAO))

    LP = Rule("LP", Production("\"", C, "\":{", A, "}"))
    LP.add(Production("\"", C, "\":{", A, "},", LP))
    LP.add(Production("\"", C, "\":[", LAO, "],", LP))
    LP.add(Production("\"", C, "\":[", LAO, "]"))

    S = Rule("S", Production("{", LP, "}"))

    text = "{ \" a \":{ \"g\":[ \" 0 \" ], \"s\":[ \" A \" ], \"i\":\" A \", \"t\":[ \" A \" ], " \
           "\"d\": true , \"e\":{ \" A \":[ [\" A \",\" 0 \"] ] }, " \
           "\"p\":{ \" A \":[ \" A \" ] } }, \" b \":[ \" a \",\" union \", \" a \" ] }"

    # Перенаправляем вывод в файл
    with open("result/log.txt", "w") as file:
        with contextlib.redirect_stdout(file):
            parse(S, text)


if __name__ == "__main__":
    main()
