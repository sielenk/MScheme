package MScheme.functions;

import MScheme.values.Value;


public class IsPairFunction
    extends UnaryPredicate
{
    public final static IsPairFunction INSTANCE
        = new IsPairFunction();

    protected boolean test(Value value)
    { return value.isPair(); }
}

