package MScheme.functions;

import MScheme.values.ValueFactory;
import MScheme.values.Value;

import MScheme.exceptions.SchemeException;


abstract class UnaryPredicate
    extends UnaryValueFunction
{
    protected abstract boolean test(
        Value fst
    ) throws SchemeException;

    protected final Value checkedCall(
        Value fst
    ) throws SchemeException
    { return ValueFactory.createBool(test(fst)); }
}
