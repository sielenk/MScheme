package MScheme;


import MScheme.expressions.*;
import MScheme.expressions.functions.*;

import MScheme.exceptions.SException;

import MScheme.environment.Environment;
import MScheme.environment.EnvironmentFactory;

import MScheme.machine.Machine;


public class Main
{
    public static void main(String argv[])
    {
        Environment env  = EnvironmentFactory.getEmpty();
        SExpr       expr =
            new SListFactory()
            .append(SyntaxFunc.BEGIN_FUNC)
            .append(
                new SListFactory()
                .append(SyntaxFunc.DEFINE_FUNC)
                .append(new SSymbol("f"))
                .append(
                    new SListFactory()
                    .append(SyntaxFunc.LAMBDA_FUNC)
                    .append(
                            new SListFactory()
                            .append(new SSymbol("x"))
                            .append(new SSymbol("y"))
                            .append(new SSymbol("z"))
                            .getList()
                    )
                    .append(new SSymbol("y"))
                    .getList()
                )
                .getList()
            )
            .append(
                new SListFactory()
                .append(new SSymbol("f"))
                .append(SBool.FALSE)
                .append(
                        new SListFactory()
                        .append(SyntaxFunc.QUOTE_FUNC)
                        .append(new SSymbol("f"))
                        .getList()
                )
                .append(SBool.TRUE)
                .getList()
            )
            .append(
                new SListFactory()
                .append(new MapFunc(EvalFunc.INSTANCE))
                .append(new SSymbol("f"))
                .append(SBool.FALSE)
                .append(SBool.TRUE)
                .getList()
            )
            .getList();

        Machine machine = new Machine(env);

        try {
            System.out.println(
                machine.evaluate(expr)
            );
        }
        catch (SException e) {
            System.err.println(
                "\n"
                + e.getSExpr()
                + " caused an "
                + e
            );
        }
    }
}
