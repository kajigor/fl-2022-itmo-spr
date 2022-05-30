package ru.itmo.formal_language;

import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.misc.ParseCancellationException;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.apache.commons.cli.*;

import java.io.*;


public class BParser {

    public enum TypeTerm {
        PROGRAM, ATOM, ARG, GOAL, DEFINITION, LIST_ARG
    }

    public BParserParser getParser() {
        return parser;
    }

    public BParserLexer getLexer() {
        return lexer;
    }

    private final BParserParser parser;
    private final BParserLexer lexer;


    public BParser(String raw) {
        // init lexer
        CharStream charStream = CharStreams.fromString(raw);
        this.lexer = new BParserLexer(charStream);
        lexer.removeErrorListeners();
        lexer.addErrorListener(ThrowingErrorListener.INSTANCE);
        CommonTokenStream tokens = new CommonTokenStream(lexer);
        // init parser
        this.parser = new BParserParser(tokens);
        parser.removeErrorListeners();
        parser.addErrorListener(ThrowingErrorListener.INSTANCE);
    }

    public String parseProgram(TypeTerm typeTerm) throws ParseCancellationException {
        ParserRuleContext programContext;
        switch (typeTerm) {
            case PROGRAM: {
                programContext = parser.start();
                 break;
            }
            case ARG: {
                programContext = parser.arg();
                break;
            }
            case ATOM: {
                programContext = parser.atom();
                break;
            }
            case GOAL: {
                programContext = parser.goal();
                break;
            }
            case LIST_ARG: {
                programContext = parser.listArg();
                break;
            }
            case DEFINITION: {
                programContext = parser.definition();
                break;
            }
            default: throw new RuntimeException("todo");
        }
        ParseTreeWalker walker = new ParseTreeWalker();

        BParserListenerImpl listener = new BParserListenerImpl();

        walker.walk(listener, programContext);

        return listener.getProgram();
    }

    public static void main(String[] args) {
        final Options options = new Options();
        Option inputOption = new Option("i", "input", true, "Path to input file.");
        inputOption.setRequired(true);
        Option outputOption = new Option("o", "output", true, "Path to output file.");
        options.addOption(inputOption);
        options.addOption(outputOption);
        CommandLineParser parser = new DefaultParser();
        try {
            // parse the command line arguments
            CommandLine line = parser.parse(options, args);
            // getting program text
            String inputFile = line.getOptionValue("i");
            String rawProgram = Utils.getStringFromFile(inputFile);

            // parse B-program
            var resultAST = new BParser(rawProgram).parseProgram(TypeTerm.PROGRAM);

            // output resulting AST
            if (line.hasOption("o")) {
                String outputFile = line.getOptionValue("output");
                Utils.outputToFile(outputFile, resultAST);
            } else {
                Utils.outputToStdout(resultAST);
            }
        }
        catch (ParseException exp) {
            HelpFormatter formatter = new HelpFormatter();
            formatter.printHelp("b-parser", options);
        } catch (IOException e) {
            System.out.println("Problems with input/output files");
        } catch (ParseCancellationException regE) {
            System.out.println("Parsing error: " + regE.getMessage());
        }
    }
}
