package MScheme.functions;

import MScheme.values.*;

import MScheme.exceptions.SchemeException;


public class CloseOutputFunction
    extends UnaryFunction
{
    public final static CloseOutputFunction INSTANCE
        = new CloseOutputFunction();
    
    protected Value checkedCall(
        Value argument
    ) throws SchemeException
    {
        argument.toPort().toOutput().close();
        return argument;
    }
}

