package ru.itmo.formal_language.dka;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.util.*;


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
    private final List<Character> alphas = new ArrayList<>();
    private final List<String> states = new ArrayList<>();

    public List<String> getStates() {
        return states;
    }

    public String getQ0() {
        return q0;
    }

    public Map<Map.Entry<String, Character>, String> getTransitionFunction() {
        return transitionFunction;
    }

    private static void initListChars(BufferedReader reader, List<Character> list) throws IOException {
        String strN = reader.readLine();
        int n = Integer.parseInt(strN);
        for (int i = 0; i < n; i++) {
            char c = reader.readLine().charAt(0);
            list.add(c);
        }
    }

    private static void initListStrings(BufferedReader reader, List<String> list) throws IOException {
        String strN = reader.readLine();
        int n = Integer.parseInt(strN);
        for (int i = 0; i < n; i++) {
            String string = reader.readLine();
            list.add(string);
        }
    }

    private final List<String> terminalStates = new ArrayList<>();
    private String q0;
    private final Map<Map.Entry<String, Character>, String> transitionFunction = new HashMap<>();

    public List<Character> getAlphabet() {
        return alphas;
    }

    public List<String> getTerminalStates() {
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

    public void init(Reader reader) throws IOException, DKAInitException {
        BufferedReader bufferedReader = new BufferedReader(reader);
        initListChars(bufferedReader, alphas);
        initListStrings(bufferedReader, states);
        if (!states.isEmpty()) {
            q0 = states.get(0);
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
