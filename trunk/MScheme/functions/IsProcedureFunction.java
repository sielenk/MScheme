package MScheme.functions;

import MScheme.values.Value;


public class IsProcedureFunction
    extends UnaryPredicate
{
    public final static IsProcedureFunction INSTANCE
        = new IsProcedureFunction();

    protected boolean test(Value value)
    { return value.isFunction(); }
}

