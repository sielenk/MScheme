package MScheme.exceptions;

import MScheme.values.Value;


public class OutputPortExpectedException
    extends PortExpectedException
{
    public OutputPortExpectedException(Value cause)
    { super(cause); }
}

