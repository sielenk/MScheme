package MScheme.exceptions;

import MScheme.values.Value;


public class InputPortExpectedException
    extends PortExpectedException
{
    public InputPortExpectedException(Value cause)
    { super(cause); }
}

