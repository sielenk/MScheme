package MScheme.functions;

import MScheme.values.ValueFactory;
import MScheme.values.Value;
import MScheme.values.List;

import MScheme.exceptions.ListExpectedException;


public class LengthFunction
    extends UnaryValueFunction
{
    public final static LengthFunction INSTANCE
        = new LengthFunction();


    protected Value checkedCall(
        Value argument
    ) throws ListExpectedException
    {
        return ValueFactory.createNumber(
            argument.toList().getLength()
        );
    }
}

