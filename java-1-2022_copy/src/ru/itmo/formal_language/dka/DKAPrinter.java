package ru.itmo.formal_language.dka;

import static guru.nidi.graphviz.model.Factory.graph;
import static guru.nidi.graphviz.model.Factory.node;
import static guru.nidi.graphviz.model.Factory.*;

import guru.nidi.graphviz.attribute.*;
import guru.nidi.graphviz.engine.Format;
import guru.nidi.graphviz.engine.Graphviz;
import guru.nidi.graphviz.model.Graph;
import guru.nidi.graphviz.model.Node;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class DKAPrinter {

    static public void print(DKA dka, String file) throws IOException {
        List<String> nodesStr = dka.getStates();
        List<String> terminalNodesStr = dka.getTerminalStates();
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
        Map<Map.Entry<String, String>, List<Character>> prettyTransitionFun = new HashMap<>();
        for (var trans : transitionFun.entrySet()) {
            var qFrom = trans.getKey().getKey();
            var alpha = trans.getKey().getValue();
            var qTo = trans.getValue();
            var tmpKey = Map.entry(qFrom, qTo);
            if (!prettyTransitionFun.containsKey(tmpKey)) {
                prettyTransitionFun.put(tmpKey, new ArrayList<>());
            }
            prettyTransitionFun.get(tmpKey).add(alpha);
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

    private DKAPrinter() {}
}
