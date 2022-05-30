package ru.itmo.formal_language;

import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.misc.ParseCancellationException;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.*;

class BParserTest {

    private static List<String> getTokens(BParserLexer lexer) {
        return lexer.getAllTokens().stream().map(Token::getText).collect(Collectors.toList());
    }

    @Test
    void testLexer() {
        // test arg
        String argProgram = "X";
        BParser argParser = new BParser(argProgram);
        assertEquals(List.of("X"), getTokens(new BParser(argProgram).getLexer()));
        // test list arg
        String listArgProgram = "X, Y, true";
        assertEquals(List.of("X", ",", "Y", ",", "true"), getTokens(new BParser(listArgProgram).getLexer()));
        // test atom
        String atomProgram = "conj(X, Y)";
        assertEquals(List.of("conj", "(", "X", ",", "Y", ")"), getTokens(new BParser(atomProgram).getLexer()));
        // simple
        String goalProgram1 = "f1(x). :- eval(X).";
        assertEquals(List.of("f1", "(", "x", ")", ".", ":-", "eval", "(", "X", ")", "."),
                getTokens(new BParser(goalProgram1).getLexer()));
    }


    @Test
    void parseRightProgram() {
        // test arg
        String argProgram = "X";
        String expectedArgOutput = "(arg (var X))";
        assertEquals(expectedArgOutput, new BParser(argProgram).parseProgram(BParser.TypeTerm.ARG));
        // test list arg
        String listArgProgram = "X, Y, true";
        String expectedListArgOutput = "(listArg (arg (var X)),(listArg (arg (var Y)),(listArg (arg true))))";
        assertEquals(expectedListArgOutput, new BParser(listArgProgram).parseProgram(BParser.TypeTerm.LIST_ARG));
        // test atom
        String atomProgram = "conj(X, Y)";
        String expectedAtomOutput = "(atom (identifier conj)((listArg (arg (var X)),(listArg (arg (var Y))))))";
        assertEquals(expectedAtomOutput, new BParser(atomProgram).parseProgram(BParser.TypeTerm.ATOM));
        // test goal
        // simple
        String goalProgram1 = "f1(x).";
        String expectedGoalProgram1 = "(goal (goalExpr (atom (identifier f1)((listArg (arg (atom (identifier x))))))).)";
        assertEquals(expectedGoalProgram1, new BParser(goalProgram1).parseProgram(BParser.TypeTerm.GOAL));
        //
        String goalProgram2 = "f1(x) \\/ f2(y).";
        String expectedGoalProgram2 = "(goal (goalExpr (goalExpr (atom (identifier f1)((listArg (arg (atom (identifier x)))))))\\/(goalExpr (atom (identifier f2)((listArg (arg (atom (identifier y)))))))).)";
        assertEquals(expectedGoalProgram2, new BParser(goalProgram2).parseProgram(BParser.TypeTerm.GOAL));
        // associativity
        String goalProgram3 = "f1(x) \\/ f2(y) \\/ f3(z).";
        String expectedGoalProgram3 = "(goal (goalExpr (goalExpr (atom (identifier f1)((listArg (arg (atom (identifier x)))))))\\/(goalExpr (goalExpr (atom (identifier f2)((listArg (arg (atom (identifier y)))))))\\/(goalExpr (atom (identifier f3)((listArg (arg (atom (identifier z))))))))).)";
        assertEquals(expectedGoalProgram3, new BParser(goalProgram3).parseProgram(BParser.TypeTerm.GOAL));
        // test definition
        String definitionProgram = "eval(x) :- eval(false).";
        String expectedDefinitionProgram = "(definition (atom (identifier eval)((listArg (arg (atom (identifier x)))))):-(goalExpr (atom (identifier eval)((listArg (arg false))))).)";
        assertEquals(expectedDefinitionProgram, new BParser(definitionProgram).parseProgram(BParser.TypeTerm.DEFINITION));
        // complex test
        String complexProgram = "eval(St, var(X), U) :- elem(X, St, U).\n" +
                "eval(St, conj(X, Y), U) :- eval(St, X, V) /\\ eval(St, X, Vv9v).\n" +
                "\n" +
                "\n" +
                "? eval(St, conj(disj(X, Y), not(var(Z))), true).";
        String expectedOutput = "(start (e (definitions (definition (atom (identifier eval)((listArg (arg (var St)),(listArg (arg (atom (identifier var)((listArg (arg (var X)))))),(listArg (arg (var U))))))):-(goalExpr (atom (identifier elem)((listArg (arg (var X)),(listArg (arg (var St)),(listArg (arg (var U)))))))).)(definitions (definition (atom (identifier eval)((listArg (arg (var St)),(listArg (arg (atom (identifier conj)((listArg (arg (var X)),(listArg (arg (var Y))))))),(listArg (arg (var U))))))):-(goalExpr (goalExpr (atom (identifier eval)((listArg (arg (var St)),(listArg (arg (var X)),(listArg (arg (var V))))))))/\\(goalExpr (atom (identifier eval)((listArg (arg (var St)),(listArg (arg (var X)),(listArg (arg (var Vv9v))))))))).)))?(goal (goalExpr (atom (identifier eval)((listArg (arg (var St)),(listArg (arg (atom (identifier conj)((listArg (arg (atom (identifier disj)((listArg (arg (var X)),(listArg (arg (var Y))))))),(listArg (arg (atom (identifier not)((listArg (arg (atom (identifier var)((listArg (arg (var Z))))))))))))))),(listArg (arg true))))))).))<EOF>)";
        assertEquals(expectedOutput, new BParser(complexProgram).parseProgram(BParser.TypeTerm.PROGRAM));
    }

    @Test
    void parseErrorProgram() {
        // test arg
        assertThrows(ParseCancellationException.class, () -> {
            new BParser("xм").parseProgram(BParser.TypeTerm.ARG);
        });
        // test list arg
        assertThrows(ParseCancellationException.class, () -> {
            new BParser("X, Y,, true").parseProgram(BParser.TypeTerm.LIST_ARG);
        });
        // test atom
        assertThrows(ParseCancellationException.class, () -> {
            new BParser("Conj(X, Y)").parseProgram(BParser.TypeTerm.ATOM);
        });
        // test goal
        assertThrows(ParseCancellationException.class, () -> {
            new BParser("f1(x)").parseProgram(BParser.TypeTerm.GOAL);
        });
        // test simple
        assertThrows(ParseCancellationException.class, () -> {
            new BParser("erunda...").parseProgram(BParser.TypeTerm.PROGRAM);
        });
        assertThrows(ParseCancellationException.class, () -> {
            new BParser("eval(X). ? eval(y)").parseProgram(BParser.TypeTerm.PROGRAM);
        });
        assertThrows(ParseCancellationException.class, () -> {
            new BParser("eval(X). ? eval(y)").parseProgram(BParser.TypeTerm.PROGRAM);
        });
    }
}