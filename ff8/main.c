#include <stdio.h>
#include <string.h>
#include "include/grammar.h"


int main(int argc, char** argv) {
    struct Grammar g;
    int i, j;
    int len;
    char str[256];

// 1 задание
//    InitGrammar(&g, 8, "*+abcwn$", 11, "PTYQJUKSNWA", 'S');
//
//    AddRule(&g, 'S', 5, "+NANA");
//    AddRule(&g, 'S', 5, "*NANA");
//    AddRule(&g, 'S', 1, "A");
//
//    AddRule(&g, 'N', 1, "n");
//
//    AddRule(&g, 'A', 4, "PQUJ");
//
//    AddRule(&g, 'P', 4, "1NTN");
//    AddRule(&g, 'P', 6, "2NTNTN");
//
//    AddRule(&g, 'T', 1, "a");
//    AddRule(&g, 'T', 1, "b");
//    AddRule(&g, 'T', 1, "c");
//
//    AddRule(&g, 'Q', 4, "1NYN");
//    AddRule(&g, 'Q', 6, "2NYNYN");
//
//    AddRule(&g, 'U', 2, "0N");
//    AddRule(&g, 'U', 4, "1NYN");
//    AddRule(&g, 'U', 6, "2NYNYN");
//
//    AddRule(&g, 'J', 2, "0N");
//    AddRule(&g, 'J', 8, "1NYWYWYN");
//    AddRule(&g, 'J', 14, "2NYWYWYNYWYWYN");
//
//    AddRule(&g, 'W', 1, "w");
//
//    AddRule(&g, 'Y', 2, "TK");
//    AddRule(&g, 'K', 2, "TK");
//    AddRule(&g, 'K', 1, "$");
//----------------------------------------------------

// 2 задание
//    E  -> T Z
//    Z -> + T Z | epsilon
//    T  -> F X
//    X -> * F X | epsilon
//    F  -> (E) | N
//    N -> 0
//    N -> 1R | .. | 9R
//    R -> N | epsilon

    InitGrammar(&g, 15, "+*()0123456789$", 7, "ETZFXNR", 'E');
    AddRule(&g, 'E', 2, "TZ");
    AddRule(&g, 'Z', 3, "+TZ");
    AddRule(&g, 'Z', 1, "$");
    AddRule(&g, 'T', 2, "FX");
    AddRule(&g, 'X', 3, "*FX");
    AddRule(&g, 'X', 1, "$");
    AddRule(&g, 'F', 3, "(E)");
    AddRule(&g, 'F', 1, "N");
    AddRule(&g, 'N', 1, "0");

    AddRule(&g, 'N', 2, "1R");
    AddRule(&g, 'N', 2, "2R");
    AddRule(&g, 'N', 2, "3R");
    AddRule(&g, 'N', 2, "4R");
    AddRule(&g, 'N', 2, "5R");
    AddRule(&g, 'N', 2, "6R");
    AddRule(&g, 'N', 2, "7R");
    AddRule(&g, 'N', 2, "8R");
    AddRule(&g, 'N', 2, "9R");

    AddRule(&g, 'R', 1, "N");
    AddRule(&g, 'R', 1, "$");


    //2. ?????????? FIRST
    FindFIRST(&g);
    //3. ?????????? FOLLOW
    FindFOLLOW(&g);
    //4. ???????? ???????
    if(!CreateTable(&g)) {
        printf("Not ll1 grammar");
        return 1;
    }
    //5. ????? FIRST, FOLLOW ? ??????? ?? ?????
    printf("\tFIRST\tFOLLOW\n");
    for (i = 0; i < g.Vn.size; ++i) {
        printf("%c\t", g.Vn.s[i]);
        PrintStr(&g.FIRST[i]);
        printf("\t");
        PrintStr(&g.FOLLOW[i]);
        printf("\n");
    }
    printf("\n");
    printf("Table:\n");
    for (j = 0; j < g.Vt.size; ++j)
        printf("\t%c", g.Vt.s[j]);
    printf("\n");
    for (i = 0; i<g.Vn.size; ++i) {
        printf("%c\t", g.Vn.s[i]);
        for (j = 0; j < g.Vt.size; ++j) {
            if (g.table[i][j] != -1) {
                printf("%c->", g.P[g.table[i][j]].a);
                PrintStr(&g.P[g.table[i][j]].b);
            }
            else
                printf("Err");
            printf("\t");
        }
        printf("\n");
    }
    printf("\n");
    //6. ?????? ? ?????????? ????
    WriteInFILE(&g);
    //7. ?????????????? ? ?????????????
    printf("Enter string: ");
    fgets(str, 256, stdin);
    len = strlen(str);
    //8. ???????
    printf("Parsing: ");
    Parse(&g, len, str);
    return 0;
}