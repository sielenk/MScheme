package MScheme.functions;

import MScheme.values.Value;


public class IsSymbolFunction
    extends UnaryPredicate
{
    public final static IsSymbolFunction INSTANCE
        = new IsSymbolFunction();

    protected boolean test(Value value)
    { return value.isSymbol(); }
}

