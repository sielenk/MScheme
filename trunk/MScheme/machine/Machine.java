package MScheme.machine;

import MScheme.environment.Environment;
import MScheme.expressions.SExpr;
import MScheme.expressions.functions.EvalFunc;
import MScheme.exceptions.SException;

public class Machine
{
    static SExpr evaluate(
        Environment environment,
        SExpr sexpr
    ) throws SException {
        ContinuationStack stack = new ContinuationStack();

        stack.push(EvalFunc.INSTANCE, environment);

        {
            Continuation cc;

            while ((cc = stack.getTop()) != null) {
                sexpr = cc.invoke(stack, sexpr);
            }
        }

        return sexpr;
    }
}
