package ru.itmo.formal_language;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.nio.file.Files;
import java.nio.file.Paths;

public class Utils {
//    public static String getStringFromStdin() throws IOException {
//        BufferedReader reader = new BufferedReader(
//                new InputStreamReader(System.in));
//        StringBuilder builder = new StringBuilder();
//        String curString = "";
//        do {
//            curString = reader.readLine();
//            builder.append(curString);
//            builder.append('\n');
//        } while (curString != null);
//        return builder.toString();
//    }

    public static String getStringFromFile(String fileName) throws IOException {
        return new String(Files.readAllBytes(Paths.get(fileName)));
    }

    public static void outputToFile(String fileName, String text) throws IOException {
        try (PrintWriter out = new PrintWriter(fileName)) {
            out.println(text);
        }
    }

    public static void outputToStdout(String text) {
        System.out.println(text);
    }

}
