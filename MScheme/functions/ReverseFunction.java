package MScheme.functions;

import MScheme.values.Value;
import MScheme.values.List;

import MScheme.exceptions.ListExpectedException;


public class ReverseFunction
    extends UnaryFunction
{
    public final static ReverseFunction INSTANCE
        = new ReverseFunction();


    protected Value checkedCall(
        Value   argument
    ) throws ListExpectedException
    { return argument.toList().getReversed(); }
}

