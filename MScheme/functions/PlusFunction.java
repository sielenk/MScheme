package MScheme.functions;

import MScheme.values.ValueFactory;
import MScheme.values.Value;
import MScheme.values.List;

import MScheme.exceptions.SchemeException;
import MScheme.exceptions.NumberExpectedException;


class Adder
    extends Reducer
{
    final static Adder INSTANCE = new Adder();

    protected Value initial()
    { return ValueFactory.createNumber(0); }

    protected Value combine(Value fst, Value snd)
        throws NumberExpectedException
    { return fst.toNumber().plus(snd.toNumber()); }
}


public class PlusFunction
    extends CheckedFunction
{
    public final static PlusFunction INSTANCE
        = new PlusFunction();

    protected Value checkedCall(List arguments)
        throws SchemeException
    { return Adder.INSTANCE.foldLeft(arguments); }
}

