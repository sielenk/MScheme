package MScheme.functions;

import MScheme.values.*;

import MScheme.exceptions.SchemeException;


public class OpenInputFunction
    extends UnaryFunction
{
    public final static OpenInputFunction INSTANCE
        = new OpenInputFunction();
    
    protected Value checkedCall(
        Value argument
    ) throws SchemeException
    {
        return ValueFactory.createInputPort(
            argument.toScmString().getString()
        );
    }
}

