package MScheme.functions;

import MScheme.values.Value;


public class IsCharFunction
    extends UnaryPredicate
{
    public final static IsCharFunction INSTANCE
        = new IsCharFunction();

    protected boolean test(Value value)
    { return value.isChar(); }
}

