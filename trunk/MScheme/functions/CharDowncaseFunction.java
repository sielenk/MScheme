package MScheme.functions;

import MScheme.values.*;

import MScheme.exceptions.SchemeException;


public class CharDowncaseFunction
    extends UnaryValueFunction
{
    public final static CharDowncaseFunction INSTANCE
        = new CharDowncaseFunction();
    
    protected Value checkedCall(
        Value argument
    ) throws SchemeException
    {
        return ValueFactory.createChar(
            Character.toLowerCase(
                argument.toChar().getChar()
            )
        );
    }
}
