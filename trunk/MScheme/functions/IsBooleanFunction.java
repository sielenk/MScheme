package MScheme.functions;

import MScheme.values.Value;


public class IsBooleanFunction
    extends UnaryPredicate
{
    public final static IsBooleanFunction INSTANCE
        = new IsBooleanFunction();

    protected boolean test(Value value)
    { return value.isBoolean(); }
}

