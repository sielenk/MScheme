package MScheme.functions;

import MScheme.values.Value;
import MScheme.values.ValueFactory;


public class IsEmptyFunction
    extends UnaryPredicate
{
    public final static IsEmptyFunction INSTANCE
        = new IsEmptyFunction();

    protected boolean test(Value value)
    { return value.eq(ValueFactory.createList()); }
}

