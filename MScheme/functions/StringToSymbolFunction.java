package MScheme.functions;

import MScheme.values.ValueFactory;
import MScheme.values.Value;
import MScheme.values.SchemeString;

import MScheme.exceptions.StringExpectedException;


public class StringToSymbolFunction
    extends UnaryValueFunction
{
    public final static StringToSymbolFunction INSTANCE
        = new StringToSymbolFunction();
    
    protected Value checkedCall(
        Value argument
    ) throws StringExpectedException
    {
        return ValueFactory.createString(
            argument.toScmString().getString()
        );
    }
}
