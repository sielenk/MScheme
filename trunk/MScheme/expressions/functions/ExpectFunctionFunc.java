package MScheme.expressions.functions;


import MScheme.expressions.SExpr;

import MScheme.exceptions.SExpectedFunctionException;

import MScheme.machine.Values;
import MScheme.machine.ContinuationStack;

import MScheme.environment.Environment;


public class ExpectFunctionFunc extends Function
{
    private Values _arguments;

    public ExpectFunctionFunc(
        Values arguments
    ) {
        super(1, 1);
        _arguments = arguments;
    }

    protected Values _call(
        ContinuationStack stack,
        Environment       environment,
        Values            arguments
    ) throws SExpectedFunctionException {
        SExpr sexpr = arguments.at(0);

        try {
            stack.push((Function)sexpr);
        } catch (ClassCastException e) {
            throw new SExpectedFunctionException(sexpr);
        }

        return _arguments;
    }
}
