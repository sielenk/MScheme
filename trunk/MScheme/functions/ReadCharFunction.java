package MScheme.functions;

import MScheme.values.*;

import MScheme.exceptions.SchemeException;


public class ReadCharFunction
    extends UnaryValueFunction
{
    public final static ReadCharFunction INSTANCE
        = new ReadCharFunction();
    
    protected Value checkedCall(
        Value argument
    ) throws SchemeException
    {
        int c = argument.toPort().toInput().readChar();

        if (c == InputPort.EOF) {
            return InputPort.EOF_VALUE;
        } else {
            return ValueFactory.createChar((char)c);
        }
    }
}
