package MScheme.functions;

import MScheme.values.*;

import MScheme.exceptions.SchemeException;


public class OpenOutputFunction
    extends UnaryValueFunction
{
    public final static OpenOutputFunction INSTANCE
        = new OpenOutputFunction();
    
    protected Value checkedCall(
        Value argument
    ) throws SchemeException
    {
        return ValueFactory.createOutputPort(
            argument.toScmString().getString()
        );
    }
}
