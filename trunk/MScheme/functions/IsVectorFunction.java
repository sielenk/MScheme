package MScheme.functions;

import MScheme.values.Value;


public class IsVectorFunction
    extends UnaryPredicate
{
    public final static IsVectorFunction INSTANCE
        = new IsVectorFunction();

    protected boolean test(Value value)
    { return value.isVector(); }
}

