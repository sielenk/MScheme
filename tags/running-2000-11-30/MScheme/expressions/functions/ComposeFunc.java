package MScheme.expressions.functions;


import MScheme.expressions.SFunction;

import MScheme.machine.Values;
import MScheme.machine.ContinuationStack;
import MScheme.environment.Environment;


public class ComposeFunc extends Function
{
    private SFunction _first;
    private SFunction _second;


    public ComposeFunc(
        SFunction first,
        SFunction second
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
