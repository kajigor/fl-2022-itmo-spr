package me.fang.parser;

import java.io.IOException;
import me.fang.parser.antlr.AutomataLexer;
import me.fang.parser.antlr.AutomataParser;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;

public class App {
    public static void main(String[] args) throws IOException {
        if (args.length > 0) {
            AutomataLexer lexer = new AutomataLexer(CharStreams.fromFileName(args[0]));
            CommonTokenStream tokens = new CommonTokenStream(lexer);
            AutomataParser parser = new AutomataParser(tokens);
            AutomataParser.StartContext tree = parser.start();
            System.out.println(tree.toStringTree());
        }
    }
}
