package MScheme.functions;

import MScheme.values.Value;
import MScheme.values.Pair;

import MScheme.exceptions.PairExpectedException;


public class CarFunction
    extends UnaryFunction
{
    public final static CarFunction INSTANCE
        = new CarFunction();


    protected Value checkedCall(
        Value argument
    ) throws PairExpectedException
    { return argument.toPair().getFirst(); }
}

