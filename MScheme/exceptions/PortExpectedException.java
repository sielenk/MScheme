package MScheme.exceptions;

import MScheme.values.Value;


public class PortExpectedException
    extends ExpectedException
{
    public PortExpectedException(Value cause)
    { super(cause); }
}

