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
    void testInitDKA() {
        try (BufferedReader reader = new BufferedReader(
                new FileReader("testResources/test1.txt")
        )) {
            // 1 test
            DKA dka = new DKA();
            dka.init(reader);
            assertEquals(Set.of('0', '1'), Set.copyOf(dka.getAlphabet()));
            assertEquals(Set.of("q_0"), Set.copyOf(dka.getTerminalStates()));
            assertEquals(Set.of("q_0", "q_1"), Set.copyOf(dka.getStates()));
            assertEquals(Map.of(
                    Map.entry("q_0", '0'), "q_1",
                    Map.entry("q_0", '1'), "q_1",
                    Map.entry("q_1", '0'), "q_0",
                    Map.entry("q_1", '1'), "q_0"), dka.getTransitionFunction()
            );
            assertEquals("q_0", dka.getQ0());
        } catch (Exception e) {
            fail();
        }
        try (BufferedReader reader = new BufferedReader(
                new FileReader("testResources/test2.txt")
        )) {
            // 2 test
            DKA dka = new DKA();
            dka.init(reader);
            assertEquals(Set.of('3', '1', '2'), Set.copyOf(dka.getAlphabet()));
            assertEquals(Set.of("q_3"), Set.copyOf(dka.getTerminalStates()));
            assertEquals(Set.of("q_0", "q_1", "q_2", "q_3"), Set.copyOf(dka.getStates()));
            assertEquals(Map.of(
                    Map.entry("q_0", '1'), "q_1",
                    Map.entry("q_1", '2'), "q_2",
                    Map.entry("q_2", '3'), "q_3"), dka.getTransitionFunction()
            );
            assertEquals("q_0", dka.getQ0());

        } catch (Exception e) {
            fail();
        }
        try (BufferedReader reader = new BufferedReader(
                new FileReader("testResources/test3.txt")
        )) {
            // 3 test
            DKA dka = new DKA();
            dka.init(reader);
            assertEquals(Set.of('a', 'b', 'c'), Set.copyOf(dka.getAlphabet()));
            assertEquals(Set.of("q_0", "q_2"), Set.copyOf(dka.getTerminalStates()));
            assertEquals(Set.of("q_0", "q_1", "q_2"), Set.copyOf(dka.getStates()));
            assertEquals(Map.of(
                    Map.entry("q_0", 'a'), "q_0",
                    Map.entry("q_0", 'b'), "q_0",
                    Map.entry("q_0", 'c'), "q_1",
                    Map.entry("q_1", 'a'), "q_1",
                    Map.entry("q_1", 'b'), "q_1",
                    Map.entry("q_1", 'c'), "q_2",
                    Map.entry("q_2", 'a'), "q_2",
                    Map.entry("q_2", 'b'), "q_2",
                    Map.entry("q_2", 'c'), "q_2"), dka.getTransitionFunction()
            );
            assertEquals("q_0", dka.getQ0());

        } catch (Exception e) {
            fail();
        }
    }

    @Test
    void testAcceptingStringDKA() {
        try (BufferedReader reader = new BufferedReader(
                new FileReader("testResources/test1.txt")
        )) {
            // 1 test
            DKA dka = new DKA();
            dka.init(reader);
            // это автомат, который принимает строки, состоящие из 0 и 1, четной длины
            assertTrue(dka.acceptString(""));
            assertTrue(dka.acceptString("01"));
            assertTrue(dka.acceptString("1101"));
            assertTrue(dka.acceptString("111011"));
            assertTrue(dka.acceptString("00101011"));
            //
            assertFalse(dka.acceptString("22"));
            assertFalse(dka.acceptString("1222"));

            assertFalse(dka.acceptString("1"));
            assertFalse(dka.acceptString("011"));
            assertFalse(dka.acceptString("11101"));
            assertFalse(dka.acceptString("1111011"));
            assertFalse(dka.acceptString("001101011"));
        } catch (Exception e) {
            fail();
        }
        try (BufferedReader reader = new BufferedReader(
                new FileReader("testResources/test2.txt")
        )) {
            // 2 test
            DKA dka = new DKA();
            dka.init(reader);
            // это автомат, принимающий только строку "123"
            assertFalse(dka.acceptString(""));
            assertFalse(dka.acceptString("1"));
            assertFalse(dka.acceptString("12"));
            assertTrue(dka.acceptString("123"));
            assertFalse(dka.acceptString("1234"));

        } catch (Exception e) {
            fail();
        }
        try (BufferedReader reader = new BufferedReader(
                new FileReader("testResources/test3.txt")
        )) {
            // 3 test
            DKA dka = new DKA();
            dka.init(reader);
            // это автомат, принимающий язык { w \in {a,b,c}* | |w|_c /= 1 }: количество букв c в слове -- не 1
            assertFalse(dka.acceptString("c"));
            assertFalse(dka.acceptString("abca"));
            assertFalse(dka.acceptString("dsfd"));
            assertFalse(dka.acceptString("1cccadd"));
            assertFalse(dka.acceptString("abbbc"));

            assertTrue(dka.acceptString("acaac"));
            assertTrue(dka.acceptString("aaabbb"));
            assertTrue(dka.acceptString("acaac"));
            assertTrue(dka.acceptString("acaccabbbc"));
            assertTrue(dka.acceptString("acaacc"));
            assertTrue(dka.acceptString("cc"));
            assertTrue(dka.acceptString("abbcc"));
            assertTrue(dka.acceptString("ccaacca"));

        } catch (Exception e) {
            fail();
        }
    }
}