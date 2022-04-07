package ru.itmo.formal_language.dka;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;


public class DKA {

    /*
     *  *  N
     *  * a_1
     *  * a_2
     *  * ..
     *  * a_N
     *  * M
     *  * q_1
     *  * q_2
     *  * ..
     *  * q_M
     *  * S
     *  * q_i1
     *  * q_i2
     *  * ..
     *  * q_iS
     *  * K
     *  * q_i_1 a_j_1 q_k_1
     *  * q_i_2 a_j_2 q_k_2
     *  * ..
     *  * q_i_K a_j_K q_k_K
     *
     * */
    private Set<Character> alphas = new HashSet<>();
    private Set<String> states = new TreeSet<>();

    public Set<String> getStates() {
        return states;
    }

    public String getQ0() {
        return q0;
    }

    public Map<Map.Entry<String, Character>, List<String>> getTransitionFunction() {
        return transitionFunction;
    }

    public static String multPresent(String l, String r) {
        return l + '_' + r;
    }

    private static Set<String> multSets(Set<String> lSet, Set<String> rSet) {
        Set<String> result = new TreeSet<>();
        for (var firstEl : lSet) {
            for (var secondEl : rSet) {
                result.add(multPresent(firstEl, secondEl));
            }
        }
        return result;
    }

    private static void addTransition(Map<Map.Entry<String, Character>, List<String>> transitionFunction,
                               String qFrom, Character symb, String qTo) {
        var key = Map.entry(qFrom, symb);
        transitionFunction.putIfAbsent(key, new ArrayList<>());
        transitionFunction.get(key).add(qTo);
    }

    enum Operation {
        MULT, UNION;
    }

    private static DKA multImpl(DKA firstDka, DKA secondDka, Operation operation) {
        DKA result = new DKA();
        result.q0 = multPresent(firstDka.q0, secondDka.q0);
        result.states = multSets(firstDka.states, secondDka.states);
        switch (operation) {
            case MULT: {
                result.terminalStates = multSets(firstDka.terminalStates, secondDka.terminalStates);
                break;
            }
            case UNION: {
                result.terminalStates = Stream.concat(
                        multSets(firstDka.terminalStates, secondDka.states).stream(),
                        multSets(firstDka.states, secondDka.terminalStates).stream()).
                        collect(Collectors.toSet());
                break;
            }
        }

        result.alphas = Stream.concat(firstDka.alphas.stream(),secondDka.alphas.stream()).collect(Collectors.toSet());

        var firstTF = firstDka.transitionFunction;
        var secondTF = secondDka.transitionFunction;
        result.transitionFunction = new HashMap<>();
        for (var firstState : firstDka.states) {
            for (var secondState : secondDka.states) {
                for (var alpha : result.alphas) {
                    var firstNextStates = firstTF.get(Map.entry(firstState, alpha));
                    var secondNextStates = secondTF.get(Map.entry(secondState, alpha));
                    //
                    if (firstNextStates == null && !firstDka.getAlphabet().contains(alpha)) {
                        firstNextStates = List.copyOf(firstDka.getStates());
                    } else if (secondNextStates == null && !secondDka.getAlphabet().contains(alpha)) {
                        secondNextStates = List.copyOf(secondDka.getStates());
                    }
                    //
                    if (firstNextStates == null || secondNextStates == null) {
                        continue;
                    }
                    for (var firstNext : firstNextStates) {
                        for (var secondNext : secondNextStates) {
                            addTransition(result.transitionFunction, multPresent(firstState, secondState),
                                    alpha, multPresent(firstNext, secondNext));
                        }
                    }
                }
            }
        }
        return result;
    }

    public static DKA mult(DKA firstDka, DKA secondDka) {
        return multImpl(firstDka, secondDka, Operation.MULT);
    }

    public static DKA union(DKA firstDka, DKA secondDka) {
        return multImpl(firstDka, secondDka, Operation.UNION);
    }

    private static void initListChars(BufferedReader reader, Set<Character> list) throws IOException {
        String strN = reader.readLine();
        int n = Integer.parseInt(strN);
        for (int i = 0; i < n; i++) {
            char c = reader.readLine().charAt(0);
            list.add(c);
        }
    }

    private static void initListStrings(BufferedReader reader, Set<String> list) throws IOException {
        String strN = reader.readLine();
        int n = Integer.parseInt(strN);
        for (int i = 0; i < n; i++) {
            String string = reader.readLine();
            list.add(string);
        }
    }

    private Set<String> terminalStates = new TreeSet<>();
    private String q0;
    private Map<Map.Entry<String, Character>, List<String>> transitionFunction = new HashMap<>();

    public Set<Character> getAlphabet() {
        return alphas;
    }

    public Set<String> getTerminalStates() {
        return terminalStates;
    }

    private void checkState(String state) throws DKAInitException {
        if (!states.contains(state)) {
            throw new DKAInitException(state + " not in states: " + states.toString());
        }
    }

    private void checkDKA() throws DKAInitException {
        checkState(q0);
        for (var state : terminalStates) {
            checkState(state);
        }
        for (var trans : transitionFunction.entrySet()) {
            var qFrom = trans.getKey().getKey();
            var alpha = trans.getKey().getValue();
            var qsTo = trans.getValue();
            checkState(qFrom);
            for (var qTo : qsTo) {
                checkState(qTo);
            }
            if (!alphas.contains(alpha)) {
                throw new DKAInitException(alpha + " not in alphabet: " + alphas.toString());
            }
        }
    }

    private DKA() {}

    public boolean isSame(DKA other) {
        return q0.equals(other.q0) &&
                terminalStates.equals(other.terminalStates) &&
                alphas.equals(other.alphas) &&
                states.equals(other.states) &&
                this.transitionFunction.equals(other.transitionFunction);
    }

    private static DKA readDKA(BufferedReader bufferedReader) throws IOException, DKAInitException {
        DKA dka = new DKA();
        initListChars(bufferedReader, dka.alphas);

        String strN = bufferedReader.readLine();
        int n = Integer.parseInt(strN);
        for (int i = 0; i < n; i++) {
            String string = bufferedReader.readLine();
            if (i == 0) {
                dka.q0 = string;
            }
            dka.states.add(string);
        }

        initListStrings(bufferedReader, dka.terminalStates);
        String strK = bufferedReader.readLine();
        int k = Integer.parseInt(strK);
        for (int i = 0; i < k; i++) {
            String tmp = bufferedReader.readLine().trim().replaceAll("[\\s]{2,}", " ") + " ";
            String[] tmps = tmp.split(" ");
            String qFrom = tmps[0];
            char alpha = tmps[1].charAt(0);
            String qTo = tmps[2];
            addTransition(dka.transitionFunction, qFrom, alpha, qTo);
        }
        //
        dka.checkDKA();
        return dka;
    }

    public static DKA init(String filename) throws IOException, DKAInitException {
        try (BufferedReader reader = new BufferedReader(
                new FileReader(filename)
        )) {
            String firstStr = reader.readLine();
            if (firstStr.equals("+") || firstStr.equals("*")) {
                DKA firstDka = readDKA(reader);
                DKA secondDka = readDKA(reader);
                return firstStr.equals("+") ? DKA.union(firstDka, secondDka) : DKA.mult(firstDka, secondDka);
            }
        }
        try (BufferedReader reader = new BufferedReader(
                new FileReader(filename)
        )) {
            return DKA.readDKA(reader);
        }
    }

    /**
     * Функция проверки принятия строки автоматом
     * @param str проверяемая строка
     * @return true если строка распознается автоматом, false - иначе
     * */
//    public boolean acceptString(String str) {
//        String curState = this.q0;
//        for (char c : str.toCharArray()) {
//            curState = this.transitionFunction.get(Map.entry(curState, c));
//            if (null == curState) {
//                return false;
//            }
//        }
//        return this.terminalStates.contains(curState);
//    }

}
