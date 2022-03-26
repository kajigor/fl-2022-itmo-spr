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
            try {
                DKA dka = DKA.init("testResources/" + testFile);
                String testEqFile = "testResources/" + testFile + "_eq.txt";
                DKAUtils.writeDKA(dka, testEqFile);
                DKA dkaTest = DKA.init(testEqFile);
                assertTrue(dka.isSame(dkaTest));
            } catch (IOException | DKAInitException e) {
                fail();
            }
        };
        fun.accept("test1.txt");
        fun.accept("test2.txt");
        fun.accept("test3.txt");
    }
}