package MScheme.expressions.functions;


import MScheme.expressions.SExpr;

import MScheme.machine.Values;
import MScheme.machine.Machine;

import MScheme.environment.Environment;


public class ConstFunc extends Function
{
    private Values _value;

    public ConstFunc(
        SExpr value
    ) {
        super(0, 1);
        _value = new Values(value);
    }

    protected Values _call(
        Machine machine,
        Values  arguments
    ) {
        return _value;
    }


    protected String defaultString()
    {
        return "[const " + _value.at(0) + "]";
    }
}
