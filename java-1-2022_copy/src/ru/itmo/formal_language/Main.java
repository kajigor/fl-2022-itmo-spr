package ru.itmo.formal_language;


import ru.itmo.formal_language.dka.DKA;
import ru.itmo.formal_language.dka.DKAInitException;
import ru.itmo.formal_language.dka.DKAPrinter;

import java.io.*;
import java.text.ParseException;

/**
 * Синтаксис для описания конечного автомата:
 * --------------------------------------
 *  N
 * a_1
 * a_2
 * ..
 * a_N
 * M
 * q_1
 * q_2
 * ..
 * q_M
 * S
 * q_i1
 * q_i2
 * ..
 * q_iS
 * K
 * q_i_1 a_j_1 q_k_1
 * q_i_2 a_j_2 q_k_2
 * ..
 * q_i_K a_j_K q_k_K
 * -----------------------------------
 * где
 * N - мощность алфавита: N :: Int
 * {a_1, .., a_N} - алфавит конечного автомата: a_i :: Character
 * a_i - любой ascii символ кроме пробела
 * M - количество различных состояний, M :: Int
 * {q_1, .., q_M} - все состояния автомата: q_i :: String, где q_1 - начальное состояние
 * q_i - строка из любых символов, не содержащая пробелов
 * S - количество терминальных состояний, S :: Int
 * K - число переходов из одного состояния в другое, K :: Int
 *
 *
 * Между строчками в одной строке может быть произвольное число пробелов.
 * Названия состояний, символы(строки) конечного автомата не должны
 * содержать пробелов.
 *
 * Примеры:
 *
 * Автомат, который принимает строчки в которых четное число нулей и единиц
 * ---------
 * 2
 * 0
 * 1
 * 2
 * q_0
 * q_1
 * 1
 * q_0
 * 4
 * q_0 0 q_1
 * q_0 1 q_1
 * q_1 0 q_0
 * q_1 1 q_0
 * --------
 *
 * Автомат, принимающий только строку 123
 * 3
 * 1
 * 2
 * 3
 * 4
 * q_0
 * q_1
 * q_2
 * q_3
 * 1
 * q_3
 * 3
 * q_0 1 q_1
 * q_1 2 q_2
 * q_2 3 q_3
 * */

public class Main {
    // <filename> [-print] [-init] [-states] [-terminal] [-trans] [-alphabet] [-accept string]
    // string in accept without quotes
    public static void main(String[] args) {
        if (args.length == 0) {
            System.out.println("<filename> [-init] [-states] [-terminal] [-trans] [-alphabet] [-accept string(without quotes)]");
            return;
        }
        String inputFile = args[0];
        try (BufferedReader reader = new BufferedReader(
                new FileReader(inputFile)
        ); BufferedWriter writer = new BufferedWriter(new FileWriter(inputFile + ".out"))) {
            DKA dka = new DKA();
            dka.init(reader);
            int i = 1;
            while (i < args.length) {
                String option = args[i];
                switch (option) {
                    case "-print" : {
                        DKAPrinter.print(dka, inputFile + ".png");
                        break;
                    }
                    case "-states" : {
                        writer.write("States: ");
                        writer.write(dka.getStates().toString());
                        writer.newLine();
                        break;
                    }
                    case "-terminal" : {
                        writer.write("Terminal states: ");
                        writer.write(dka.getTerminalStates().toString());
                        writer.newLine();
                        break;
                    }
                    case "-trans" : {
                        writer.write("Transition Function: ");
                        writer.write(dka.getTransitionFunction().toString());
                        writer.newLine();
                        break;
                    }
                    case "-alphabet" : {
                        writer.write("Alphabet: ");
                        writer.write(dka.getAlphabet().toString());
                        writer.newLine();
                        break;
                    }
                    case "-accept" : {
                        if (i + 1 >= args.length)
                            throw new RuntimeException("Expected string after -accept");
                        if (dka.acceptString(args[i + 1])) {
                            writer.write('\"' + args[i + 1] + '\"' + " was accepted");
                        } else {
                            writer.write('\"' + args[i + 1] + '\"' + " wasn't accepted");
                        }
                        writer.newLine();
                        i++;
                    }
                }
                i++;
            }
        } catch (IOException e) {
            System.out.println("Check names your files");
        } catch (DKAInitException e1) {
            System.out.println(e1.getMessage());
        } catch (RuntimeException e2) {
            System.out.println(e2.getMessage());
        }
    }
}
