package MScheme.functions;

import MScheme.values.ValueFactory;
import MScheme.values.Value;
import MScheme.values.List;

import MScheme.exceptions.SchemeException;
import MScheme.exceptions.NumberExpectedException;


class Multiplier
    extends Reducer
{
    final static Multiplier INSTANCE = new Multiplier();

    protected Value initial()
    { return ValueFactory.createNumber(1); }

    protected Value combine(Value fst, Value snd)
        throws NumberExpectedException
    { return fst.toNumber().times(snd.toNumber()); }
}


public class TimesFunction
    extends CheckedFunction
{
    public final static TimesFunction INSTANCE
        = new TimesFunction();

    protected Value checkedCall(List arguments)
        throws SchemeException
    { return Multiplier.INSTANCE.foldLeft(arguments); }
}

