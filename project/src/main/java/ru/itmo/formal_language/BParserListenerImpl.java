package ru.itmo.formal_language;

import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.TerminalNode;

public class BParserListenerImpl extends BParserBaseListener {
    StringBuilder builder = new StringBuilder();

    @Override public void enterStart(BParserParser.StartContext ctx) { builder.append("start "); }
    @Override public void enterE(BParserParser.EContext ctx) { builder.append("e ");}

    @Override public void enterDefinitions(BParserParser.DefinitionsContext ctx) { builder.append("definitions ");}

    @Override public void enterDefinition(BParserParser.DefinitionContext ctx) { builder.append("definition "); }

    @Override public void enterAtom(BParserParser.AtomContext ctx) { builder.append("atom ");}

    @Override public void enterListArg(BParserParser.ListArgContext ctx) { builder.append("listArg ");}

    @Override public void enterArg(BParserParser.ArgContext ctx) { builder.append("arg ");}

    @Override public void enterVar(BParserParser.VarContext ctx) { builder.append("var ");}

    @Override public void enterIdentifier(BParserParser.IdentifierContext ctx) { builder.append("identifier "); }

    @Override public void enterGoal(BParserParser.GoalContext ctx) { builder.append("goal "); }

    @Override public void enterGoalExpr(BParserParser.GoalExprContext ctx) { builder.append("goalExpr "); }

    @Override public void enterEveryRule(ParserRuleContext ctx) { builder.append('('); }

    @Override public void exitEveryRule(ParserRuleContext ctx) { builder.append(')'); }

    @Override public void visitTerminal(TerminalNode node) {
        builder.append(node.getSymbol().getText());
    }

    public String getProgram() {
        return builder.toString();
    }
}
