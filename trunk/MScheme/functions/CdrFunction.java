package MScheme.functions;

import MScheme.values.Value;
import MScheme.values.Pair;

import MScheme.exceptions.PairExpectedException;


public class CdrFunction
    extends UnaryFunction
{
    public final static CdrFunction INSTANCE
        = new CdrFunction();


    protected Value checkedCall(
        Value argument
    ) throws PairExpectedException
    { return argument.toPair().getSecond(); }
}

