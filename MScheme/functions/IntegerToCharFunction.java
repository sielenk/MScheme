package MScheme.functions;

import MScheme.values.*;

import MScheme.exceptions.SchemeException;


public class IntegerToCharFunction
    extends UnaryValueFunction
{
    public final static IntegerToCharFunction INSTANCE
        = new IntegerToCharFunction();
    
    protected Value checkedCall(
        Value argument
    ) throws SchemeException
    {
        return ValueFactory.createChar(
            (char)argument.toNumber().getInteger()
        );
    }
}
