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

        stack.push(EvalFunc.INSTANCE);

        while (!stack.isEmpty()) {
            sexpr = stack.getTop().invoke(stack, sexpr);
        }

        return sexpr;
    }
}
