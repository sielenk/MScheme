package MScheme.machine;

import MScheme.environment.Environment;
import MScheme.expressions.SExpr;
import MScheme.expressions.functions.EvalFunc;
import MScheme.exceptions.SException;

public class Machine
{
    public static SExpr evaluate(
        Environment environment,
        SExpr       sexpr
    ) throws SException {
        ContinuationStack stack = new ContinuationStack(environment);
        Values            accu  = new Values(sexpr);

        stack.push(
            environment,
            EvalFunc.INSTANCE
        );

        while (!stack.isEmpty()) {
            accu = stack.getTop().invoke(stack, accu);
        }

        return accu.at(0);
    }
}
