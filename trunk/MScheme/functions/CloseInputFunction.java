package MScheme.functions;

import MScheme.values.*;

import MScheme.exceptions.SchemeException;


public class CloseInputFunction
    extends UnaryValueFunction
{
    public final static CloseInputFunction INSTANCE
        = new CloseInputFunction();
    
    protected Value checkedCall(
        Value argument
    ) throws SchemeException
    {
        argument.toPort().toInput().close();
        return argument;
    }
}

