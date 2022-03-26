package ru.itmo.formal_language.dka;

import static guru.nidi.graphviz.model.Factory.graph;
import static guru.nidi.graphviz.model.Factory.node;
import static guru.nidi.graphviz.model.Factory.*;

import guru.nidi.graphviz.attribute.*;
import guru.nidi.graphviz.engine.Format;
import guru.nidi.graphviz.engine.Graphviz;
import guru.nidi.graphviz.model.Graph;
import guru.nidi.graphviz.model.Node;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.*;

public class DKAUtils {

    private DKAUtils() {}

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

    public static void writeDKA(DKA dka, String file) throws IOException {
        try (BufferedWriter writer = new BufferedWriter(new FileWriter(file))) {
            // write alphabet
            Set<Character> alphabet = dka.getAlphabet();
            writer.write(String.valueOf(alphabet.size()));
            writer.newLine();
            for (var alpha : alphabet) {
                writer.write(alpha);
                writer.newLine();
            }
            // write states
            Set<String> dkaStates = dka.getStates();
            writer.write(String.valueOf(dkaStates.size()));
            writer.newLine();
            for (var state : dkaStates) {
                writer.write(state);
                writer.newLine();
            }
            // write terminal states
            Set<String> terminalStates = dka.getTerminalStates();
            writer.write(String.valueOf(terminalStates.size()));
            writer.newLine();
            for (var state : terminalStates) {
                writer.write(state);
                writer.newLine();
            }
            // write transition function
            var transitionFunction = dka.getTransitionFunction();
            writer.write(String.valueOf(transitionFunction.size()));
            writer.newLine();
            for (var qCharQ : transitionFunction.entrySet()) {
                for (var qTo : qCharQ.getValue()) {
                    writer.write(qCharQ.getKey().getKey());
                    writer.write(' ');
                    writer.write(qCharQ.getKey().getValue());
                    writer.write(' ');
                    writer.write(qTo);
                    writer.newLine();
                }
            }
        }
    }

    public static void print(DKA dka, String file) throws IOException {
        Set<String> nodesStr = dka.getStates();
        Set<String> terminalNodesStr = dka.getTerminalStates();
        String initNode = dka.getQ0();
        Map<String, Node> strToNodes = new HashMap<>();

        for (var nodeStr : nodesStr) {
            if (nodeStr.equals(initNode)) {
                if (terminalNodesStr.contains(nodeStr)) {
                    strToNodes.put(nodeStr, node(nodeStr).with(Style.FILLED, Color.hsv(.7, .3, 1.0),
                            Label.html("<b>" + nodeStr + "</b><br/>"), Color.rgb("1020d0").font()));
                } else {
                    strToNodes.put(nodeStr, node(nodeStr).with(Label.html("<b>" + nodeStr + "</b><br/>"), Color.rgb("1020d0").font()));
                }
            } else if (terminalNodesStr.contains(nodeStr)) {
                strToNodes.put(nodeStr, node(nodeStr).with(Style.FILLED, Color.hsv(.7, .3, 1.0)));
            } else {
                strToNodes.put(nodeStr, node(nodeStr));
            }
        }
        //
        var transitionFun = dka.getTransitionFunction();
        Map<Map.Entry<String, String>, Set<Character>> prettyTransitionFun = new HashMap<>();
        for (var trans : transitionFun.entrySet()) {
            var qFrom = trans.getKey().getKey();
            var alpha = trans.getKey().getValue();
            for (var qTo : trans.getValue()) {
                var tmpKey = Map.entry(qFrom, qTo);
                if (!prettyTransitionFun.containsKey(tmpKey)) {
                    prettyTransitionFun.put(tmpKey, new TreeSet<>());
                }
                prettyTransitionFun.get(tmpKey).add(alpha);
            }
        }
        Graph g = graph(file).directed();
        for (var trans : prettyTransitionFun.entrySet()) {
            var qFrom = trans.getKey().getKey();
            var qTo = trans.getKey().getValue();
            var alphas = trans.getValue();
            g = g.with(strToNodes.get(qFrom).
                    link(to(strToNodes.get(qTo)).
                            with(Label.of(alphas.toString()))));
        }
        Graphviz.fromGraph(g).width(900).render(Format.PNG).toFile(new File(file));
    }
}
