package MScheme.functions;

import MScheme.values.Value;


public class IsStringFunction
    extends UnaryPredicate
{
    public final static IsStringFunction INSTANCE
        = new IsStringFunction();

    protected boolean test(Value value)
    { return value.isString(); }
}

