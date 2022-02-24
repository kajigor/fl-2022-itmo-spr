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
    private List<String> alphas = new ArrayList<>();
    private List<String> states = new ArrayList<>();

    public List<String> getStates() {
        return states;
    }

    public String getQ_0() {
        return q_0;
    }

    public Map<Map.Entry<String, String>, String> getTransitionFunction() {
        return transitionFunction;
    }

    private static void initListStrings(BufferedReader reader, List<String> list) throws IOException {
        String N_string = reader.readLine();
        int N = Integer.valueOf(N_string);
        for (int i = 0; i < N; i++) {
            String string = reader.readLine();
            list.add(string);
        }
    }

    private List<String> terminalStates = new ArrayList<>();
    private String q_0;
    private Map<Map.Entry<String, String>, String> transitionFunction = new HashMap<>();

    public List<String> getAlphabet() {
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
        checkState(q_0);
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
        initListStrings(bufferedReader, alphas);
        initListStrings(bufferedReader, states);
        if (!states.isEmpty()) {
            q_0 = states.get(0);
        }
        initListStrings(bufferedReader, terminalStates);
        String K_string = bufferedReader.readLine();
        int K = Integer.valueOf(K_string);
        for (int i = 0; i < K; i++) {
            String tmp = bufferedReader.readLine().trim().replaceAll("[\\s]{2,}", " ") + " ";
            String[] tmps = tmp.split(" ");
            String q_from = tmps[0];
            String alpha = tmps[1];
            String q_to = tmps[2];
            transitionFunction.put(new AbstractMap.SimpleEntry<>(q_from, alpha), q_to);
        }
        //
        checkDKA();
    }

}
