package MScheme.exceptions;

import MScheme.Value;


public class PortExpected
    extends TypeError
{
    public PortExpected(Value cause)
    { super(cause); }
}
