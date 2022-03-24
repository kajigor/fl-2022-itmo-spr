package ru.itmo.formal_language.dka;

import java.io.BufferedReader;
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
    private Set<Character> alphas = new TreeSet<>();
    private Set<String> states = new TreeSet<>();

    public Set<String> getStates() {
        return states;
    }

    public String getQ0() {
        return q0;
    }

    public Map<Map.Entry<String, Character>, String> getTransitionFunction() {
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

    public static DKA mult(DKA firstDka, DKA secondDka) {
        DKA result = new DKA();
        result.q0 = multPresent(firstDka.q0, secondDka.q0);
        result.states = multSets(firstDka.states, secondDka.states);
        result.terminalStates = multSets(firstDka.terminalStates, secondDka.terminalStates);
        result.alphas = Stream.concat(firstDka.alphas.stream(),secondDka.alphas.stream()).collect(Collectors.toSet());

        Map<Map.Entry<String, Character>, String> firstTF = firstDka.transitionFunction;
        Map<Map.Entry<String, Character>, String> secondTF = secondDka.transitionFunction;
        result.transitionFunction = new HashMap<>();
        for (var firstState : firstDka.states) {
            for (var secondState : secondDka.states) {
                for (var alpha : result.alphas) {
                    String firstNext = firstTF.get(Map.entry(firstState, alpha));
                    String secondNext = secondTF.get(Map.entry(secondState, alpha));
                    if (firstNext != null && secondNext != null) {
                        result.transitionFunction.put(Map.entry(multPresent(firstState, secondState), alpha),
                                multPresent(firstNext, secondNext));
                    }
                }
            }
        }
        return result;
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
    private Map<Map.Entry<String, Character>, String> transitionFunction = new HashMap<>();

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
            var qTo = trans.getValue();
            checkState(qFrom);
            checkState(qTo);
            if (!alphas.contains(alpha)) {
                throw new DKAInitException(alpha + " not in alphabet: " + alphas.toString());
            }
        }
    }

    public DKA() {}

    public boolean isSame(DKA other) {
        return q0.equals(other.q0) &&
                terminalStates.equals(other.terminalStates) &&
                alphas.equals(other.alphas) &&
                states.equals(other.states) &&
                this.transitionFunction.equals(other.transitionFunction);
    }

    public void init(Reader reader) throws IOException, DKAInitException {
        BufferedReader bufferedReader = new BufferedReader(reader);
        initListChars(bufferedReader, alphas);

        String strN = bufferedReader.readLine();
        int n = Integer.parseInt(strN);
        for (int i = 0; i < n; i++) {
            String string = bufferedReader.readLine();
            if (i == 0) {
                q0 = string;
            }
            states.add(string);
        }

        initListStrings(bufferedReader, terminalStates);
        String strK = bufferedReader.readLine();
        int k = Integer.parseInt(strK);
        for (int i = 0; i < k; i++) {
            String tmp = bufferedReader.readLine().trim().replaceAll("[\\s]{2,}", " ") + " ";
            String[] tmps = tmp.split(" ");
            String qFrom = tmps[0];
            char alpha = tmps[1].charAt(0);
            String qTo = tmps[2];
            transitionFunction.put(new AbstractMap.SimpleEntry<>(qFrom, alpha), qTo);
        }
        //
        checkDKA();
    }

    /**
     * Функция проверки принятия строки автоматом
     * @param str проверяемая строка
     * @return true если строка распознается автоматом, false - иначе
     * */
    public boolean acceptString(String str) {
        String curState = this.q0;
        for (char c : str.toCharArray()) {
            curState = this.transitionFunction.get(Map.entry(curState, c));
            if (null == curState) {
                return false;
            }
        }
        return this.terminalStates.contains(curState);
    }

}
