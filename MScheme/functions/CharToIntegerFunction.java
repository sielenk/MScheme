package MScheme.functions;

import MScheme.values.*;

import MScheme.exceptions.SchemeException;


public class CharToIntegerFunction
    extends UnaryValueFunction
{
    public final static CharToIntegerFunction INSTANCE
        = new CharToIntegerFunction();
    
    protected Value checkedCall(
        Value argument
    ) throws SchemeException
    {
        return ValueFactory.createNumber(
            argument.toChar().getChar()
        );
    }
}
