package MScheme.expressions.functions;


import MScheme.expressions.SExpr;
import MScheme.expressions.SBool;

import MScheme.machine.Values;
import MScheme.machine.ContinuationStack;
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
        ContinuationStack stack,
        Environment       environment,
        Values            arguments
    ) {
        return _value;
    }


    protected String defaultString()
    {
        return "[const " + _value.at(0) + "]";
    }
}
