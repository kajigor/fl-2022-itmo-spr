package ru.itmo.formal_language.dka;

import org.junit.jupiter.api.Test;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;

class DKATest {

    @Test
    void testDKA() {
        try (BufferedReader reader = new BufferedReader(
                new FileReader("/home/ivankozlov98/Learning/Formal_language/fl-2022-itmo-spr/java-1-2022/testResources/test1.txt")
        )) {
            // 1 test
            DKA dka = new DKA();
            dka.init(reader);
            assertEquals(Set.of("0", "1"), Set.copyOf(dka.getAlphabet()));
            assertEquals(Set.of("q_0"), Set.copyOf(dka.getTerminalStates()));
            assertEquals(Set.of("q_0", "q_1"), Set.copyOf(dka.getStates()));
            assertEquals(Map.of(
                    Map.entry("q_0", "0"), "q_1",
                    Map.entry("q_0", "1"), "q_1",
                    Map.entry("q_1", "0"), "q_0",
                    Map.entry("q_1", "1"), "q_0"), dka.getTransitionFunction()
            );
            assertEquals("q_0", dka.getQ_0());
        } catch (Exception e) {
            fail();
        }
        try (BufferedReader reader = new BufferedReader(
                new FileReader("/home/ivankozlov98/Learning/Formal_language/fl-2022-itmo-spr/java-1-2022/testResources/test2.txt")
        )) {
            // 2 test
            DKA dka = new DKA();
            dka.init(reader);
            assertEquals(Set.of("3", "1", "2"), Set.copyOf(dka.getAlphabet()));
            assertEquals(Set.of("q_3"), Set.copyOf(dka.getTerminalStates()));
            assertEquals(Set.of("q_0", "q_1", "q_2", "q_3"), Set.copyOf(dka.getStates()));
            assertEquals(Map.of(
                    Map.entry("q_0", "1"), "q_1",
                    Map.entry("q_1", "2"), "q_2",
                    Map.entry("q_2", "3"), "q_3"), dka.getTransitionFunction()
            );
            assertEquals("q_0", dka.getQ_0());

        } catch (Exception e) {
            fail();
        }
    }
}