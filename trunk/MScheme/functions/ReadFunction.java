package MScheme.functions;

import MScheme.values.*;

import MScheme.exceptions.SchemeException;


public class ReadFunction
    extends UnaryValueFunction
{
    public final static ReadFunction INSTANCE
        = new ReadFunction();
    
    protected Value checkedCall(
        Value argument
    ) throws SchemeException
    { return argument.toPort().toInput().read(); }
}
