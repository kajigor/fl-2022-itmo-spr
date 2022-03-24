package ru.itmo.formal_language.dka;

import org.junit.jupiter.api.Test;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.function.Consumer;
import java.util.function.Function;

import static org.junit.jupiter.api.Assertions.*;

class DKAUtilsTest {

    @Test
    void writeDKA() {
        Consumer<String> fun = testFile -> {
            try (BufferedReader reader = new BufferedReader(
                    new FileReader("testResources/" + testFile)
            )) {
                DKA dka = new DKA();
                dka.init(reader);
                String testEqFile = "testResources/" + testFile + "_eq.txt";
                DKAUtils.writeDKA(dka, testEqFile);
                try (BufferedReader readerTest = new BufferedReader(
                        new FileReader(testEqFile)
                )) {
                    DKA dkaTest = new DKA();
                    dkaTest.init(readerTest);
                    assertTrue(dka.isSame(dkaTest));
                }

            } catch (IOException | DKAInitException e) {
                fail();
            }
        };
        fun.accept("test1.txt");
        fun.accept("test2.txt");
        fun.accept("test3.txt");
    }
}