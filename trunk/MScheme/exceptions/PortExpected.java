package MScheme.exceptions;

import MScheme.values.Value;


public class PortExpected
    extends TypeError
{
    public PortExpected(Value cause)
    { super(cause); }
}
