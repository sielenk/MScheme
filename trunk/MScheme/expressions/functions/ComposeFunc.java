package MScheme.expressions.functions;


import MScheme.expressions.SExpr;
import MScheme.expressions.SBool;

import MScheme.machine.Values;
import MScheme.machine.ContinuationStack;
import MScheme.environment.Environment;


public class ComposeFunc extends Function
{
    private Function _first;
    private Function _second;


    public ComposeFunc(
        Function first,
        Function second
    ) {
        super(
            first.getMinArity(),
            first.getMaxArity()
        );

        _first  = first;
        _second = second;
    }


    protected Values _call(
        ContinuationStack stack,
        Environment       environment,
        Values            arguments
    ) {
        stack.push(
            environment,
            _second
        );

        stack.push(
            environment,
            _first
        );

        return arguments;
    }


    protected String defaultString()
    {
        return "[compose " + _second + " " + _first + "]";
    }
}
