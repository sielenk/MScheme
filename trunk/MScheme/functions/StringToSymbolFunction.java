package MScheme.functions;

import MScheme.values.*;

import MScheme.exceptions.SchemeException;


public class StringToSymbolFunction
    extends UnaryFunction
{
    public final static StringToSymbolFunction INSTANCE
        = new StringToSymbolFunction();
    
    protected Value checkedCall(
        Value argument
    ) throws SchemeException
    {
        return ValueFactory.createString(
            argument.toScmString().getString()
        );
    }
}

