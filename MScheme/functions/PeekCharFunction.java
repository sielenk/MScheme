package MScheme.functions;

import MScheme.values.*;

import MScheme.exceptions.SchemeException;


public class PeekCharFunction
    extends UnaryValueFunction
{
    public final static PeekCharFunction INSTANCE
        = new PeekCharFunction();
    
    protected Value checkedCall(
        Value argument
    ) throws SchemeException
    {
        int c = argument.toPort().toInput().peekChar();

        if (c == InputPort.EOF) {
            return InputPort.EOF_VALUE;
        } else {
            return ValueFactory.createChar((char)c);
        }
    }
}
