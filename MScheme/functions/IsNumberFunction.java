package MScheme.functions;

import MScheme.values.Value;


public class IsNumberFunction
    extends UnaryPredicate
{
    public final static IsNumberFunction INSTANCE
        = new IsNumberFunction();

    protected boolean test(Value value)
    { return value.isNumber(); }
}

