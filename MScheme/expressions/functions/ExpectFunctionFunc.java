package MScheme.expressions.functions;

import MScheme.expressions.SExpr;
import MScheme.expressions.SValues;

import MScheme.exceptions.SExpectedFunctionException;
import MScheme.machine.ContinuationStack;
import MScheme.environment.Environment;

public class ExpectFunctionFunc extends Function
{
    private SValues _arguments;

    public ExpectFunctionFunc(
        SValues arguments
    ) {
        _arguments = arguments;
    }

    public SExpr call(
        ContinuationStack stack,
        Environment       environment,
        SExpr             sexpr
    ) throws SExpectedFunctionException {
        try {
            stack.push((Function)sexpr);
            return _arguments;
        } catch (ClassCastException e) {
            throw new SExpectedFunctionException(sexpr);
        }
    }
}
