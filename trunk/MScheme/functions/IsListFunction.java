package MScheme.functions;

import MScheme.values.Value;


public class IsListFunction
    extends UnaryPredicate
{
    public final static IsListFunction INSTANCE
        = new IsListFunction();

    protected boolean test(Value value)
    { return value.isList(); }
}

