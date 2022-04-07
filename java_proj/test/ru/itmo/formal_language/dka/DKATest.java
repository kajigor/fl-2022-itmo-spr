package ru.itmo.formal_language.dka;

import org.junit.jupiter.api.Test;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;

class DKATest {

    @Test
    void testMultDKA() {
        // norm test
        try {
            DKA resMult = DKA.init("testResources/test_mult1.txt");
            DKA resUnion = DKA.init("testResources/test_sum1.txt");
            assertEquals(Set.of('0', '1'), resMult.getAlphabet());
            assertEquals(Set.of('0', '1'), resUnion.getAlphabet());
            var expectedStates = Set.of(
                    DKA.multPresent("s1", "s2"),
                    DKA.multPresent("s1", "q2"),
                    DKA.multPresent("s1", "t21"),
                    DKA.multPresent("s1", "t22"),
                    DKA.multPresent("t1", "s2"),
                    DKA.multPresent("t1", "q2"),
                    DKA.multPresent("t1", "t21"),
                    DKA.multPresent("t1", "t22")
            );
            assertEquals(expectedStates, resMult.getStates());
            assertEquals(expectedStates, resUnion.getStates());

            Map<Map.Entry<String, Character>, List<String>> trueTransition = new HashMap<>();
            trueTransition.put(Map.entry(DKA.multPresent("s1", "s2"), '0'), List.of(DKA.multPresent("s1", "q2")));
            trueTransition.put(Map.entry(DKA.multPresent("s1", "s2"), '1'), List.of(DKA.multPresent("t1", "s2")));
            trueTransition.put(Map.entry(DKA.multPresent("s1", "q2"), '0'), List.of(DKA.multPresent("s1", "q2")));
            trueTransition.put(Map.entry(DKA.multPresent("s1", "q2"), '1'), List.of(DKA.multPresent("t1", "t21")));
            trueTransition.put(Map.entry(DKA.multPresent("s1", "t21"), '0'),List.of( DKA.multPresent("s1", "t21")));
            trueTransition.put(Map.entry(DKA.multPresent("s1", "t21"), '1'), List.of( DKA.multPresent("t1", "t21")));
            trueTransition.put(Map.entry(DKA.multPresent("s1", "t22"), '0'), List.of( DKA.multPresent("s1", "t22")));
            trueTransition.put(Map.entry(DKA.multPresent("s1", "t22"), '1'), List.of( DKA.multPresent("t1", "t22")));
            trueTransition.put(Map.entry(DKA.multPresent("t1", "s2"), '0'), List.of(DKA.multPresent("t1", "q2")));
            trueTransition.put(Map.entry(DKA.multPresent("t1", "s2"), '1'), List.of(DKA.multPresent("t1", "s2")));
            trueTransition.put(Map.entry(DKA.multPresent("t1", "q2"), '0'), List.of(DKA.multPresent("t1", "q2")));
            trueTransition.put(Map.entry(DKA.multPresent("t1", "q2"), '1'), List.of(DKA.multPresent("t1", "t21")));
            trueTransition.put(Map.entry(DKA.multPresent("t1", "t21"), '0'),List.of( DKA.multPresent("t1", "t21")));
            trueTransition.put(Map.entry(DKA.multPresent("t1", "t21"), '1'),List.of( DKA.multPresent("t1", "t21")));
            trueTransition.put(Map.entry(DKA.multPresent("t1", "t22"), '0'),List.of( DKA.multPresent("t1", "t22")));
            trueTransition.put(Map.entry(DKA.multPresent("t1", "t22"), '1'),List.of( DKA.multPresent("t1", "t22")));
            assertEquals(trueTransition, resMult.getTransitionFunction());
            assertEquals(trueTransition, resUnion.getTransitionFunction());

            assertEquals(DKA.multPresent("s1", "s2"), resMult.getQ0());
            assertEquals(DKA.multPresent("s1", "s2"), resUnion.getQ0());

            assertEquals(Set.of(
                    DKA.multPresent("t1", "t21"),
                    DKA.multPresent("t1", "t22")
                    ),
                    resMult.getTerminalStates());

            assertEquals(Set.of(
                    DKA.multPresent("s1", "t21"),
                    DKA.multPresent("s1", "t22"),
                    DKA.multPresent("t1", "s2"),
                    DKA.multPresent("t1", "q2"),
                    DKA.multPresent("t1", "t21"),
                    DKA.multPresent("t1", "t22")
                    ),
                    resUnion.getTerminalStates());

        } catch (IOException | DKAInitException e) {
            fail();
        }
    }

    @Test
    void testInitDKA() {
        try {
            // 1 test
            DKA dka = DKA.init("testResources/test1.txt");
            assertEquals(Set.of('0', '1'), dka.getAlphabet());
            assertEquals(Set.of("q_0"), dka.getTerminalStates());
            assertEquals(Set.of("q_0", "q_1"), dka.getStates());
            assertEquals(Map.of(
                    Map.entry("q_0", '0'), List.of("q_1"),
                    Map.entry("q_0", '1'), List.of("q_1"),
                    Map.entry("q_1", '0'), List.of("q_0"),
                    Map.entry("q_1", '1'), List.of("q_0")), dka.getTransitionFunction()
            );
            assertEquals("q_0", dka.getQ0());
        } catch (Exception e) {
            fail();
        }
        try {
            // 2 test
            DKA dka = DKA.init("testResources/test2.txt");
            assertEquals(Set.of('3', '1', '2'), (dka.getAlphabet()));
            assertEquals(Set.of("q_3"), (dka.getTerminalStates()));
            assertEquals(Set.of("q_0", "q_1", "q_2", "q_3"), (dka.getStates()));
            assertEquals(Map.of(
                    Map.entry("q_0", '1'), List.of("q_1"),
                    Map.entry("q_1", '2'), List.of("q_2"),
                    Map.entry("q_2", '3'), List.of("q_3")), dka.getTransitionFunction()
            );
            assertEquals("q_0", dka.getQ0());

        } catch (Exception e) {
            fail();
        }
        try {
            // 3 test
            DKA dka = DKA.init("testResources/test3.txt");
            assertEquals(Set.of('a', 'b', 'c'), dka.getAlphabet());
            assertEquals(Set.of("q_0", "q_2"), dka.getTerminalStates());
            assertEquals(Set.of("q_0", "q_1", "q_2"), dka.getStates());
            assertEquals(Map.of(
                    Map.entry("q_0", 'a'), List.of("q_0"),
                    Map.entry("q_0", 'b'), List.of("q_0"),
                    Map.entry("q_0", 'c'), List.of("q_1"),
                    Map.entry("q_1", 'a'), List.of("q_1"),
                    Map.entry("q_1", 'b'), List.of("q_1"),
                    Map.entry("q_1", 'c'), List.of("q_2"),
                    Map.entry("q_2", 'a'), List.of("q_2"),
                    Map.entry("q_2", 'b'), List.of("q_2"),
                    Map.entry("q_2", 'c'), List.of("q_2")), dka.getTransitionFunction()
            );
            assertEquals("q_0", dka.getQ0());

        } catch (Exception e) {
            fail();
        }
        // test NKA
        try {
            // 3 test
            DKA dka = DKA.init("testResources/test7.txt");
            assertEquals(Set.of('0', '1'), dka.getAlphabet());
            assertEquals(Set.of("q_0", "q_2"), dka.getTerminalStates());
            assertEquals(Set.of("q_0", "q_1", "q_2"), dka.getStates());
            assertEquals(Map.of(
                    Map.entry("q_0", '0'), List.of("q_1"),
                    Map.entry("q_0", '1'), List.of("q_2", "q_1"),
                    Map.entry("q_1", '0'), List.of("q_0"),
                    Map.entry("q_1", '1'), List.of("q_0")).entrySet(), dka.getTransitionFunction().entrySet()
            );
            assertEquals("q_0", dka.getQ0());

        } catch (Exception e) {
            fail();
        }
    }
}