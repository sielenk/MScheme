package MScheme.functions;

import MScheme.values.*;

import MScheme.exceptions.SchemeException;


public class ReadFunction
    extends UnaryFunction
{
    public final static ReadFunction INSTANCE
        = new ReadFunction();
    
    protected Value checkedCall(
        Value argument
    ) throws SchemeException
    { return argument.toPort().toInput().read(); }
}

