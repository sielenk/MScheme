package MScheme.functions;

import MScheme.values.*;

import MScheme.exceptions.SchemeException;


public class CharUpcaseFunction
    extends UnaryValueFunction
{
    public final static CharUpcaseFunction INSTANCE
        = new CharUpcaseFunction();
    
    protected Value checkedCall(
        Value argument
    ) throws SchemeException
    {
        return ValueFactory.createChar(
            Character.toUpperCase(
                argument.toChar().getChar()
            )
        );
    }
}
