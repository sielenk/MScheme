package MScheme.expressions.functions;


import MScheme.expressions.SFunction;

import MScheme.util.Values;
import MScheme.machine.Machine;

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
        Machine machine,
        Values  arguments
    ) {
        machine.push(_second);
        machine.push(_first );

        return arguments;
    }


    protected String defaultString()
    {
        return "[compose " + _second + " " + _first + "]";
    }
}
