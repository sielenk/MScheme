package MScheme.functions;

import MScheme.values.ValueFactory;
import MScheme.values.Value;

import MScheme.exceptions.SchemeException;


abstract class BinaryPredicate
    extends BinaryFunction
{
    protected abstract boolean test(
        Value fst,
        Value snd
    ) throws SchemeException;

    protected final Value checkedCall(
        Value fst,
        Value snd
    ) throws SchemeException
    { return ValueFactory.createBool(test(fst, snd)); }
}

