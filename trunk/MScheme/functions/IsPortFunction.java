package MScheme.functions;

import MScheme.values.Value;


public class IsPortFunction
    extends UnaryPredicate
{
    public final static IsPortFunction INSTANCE
        = new IsPortFunction();

    protected boolean test(Value value)
    { return value.isPort(); }
}

