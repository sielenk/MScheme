package MScheme.exceptions;

import MScheme.Value;


public class EnvironmentExpected
    extends TypeError
{
    public EnvironmentExpected(Value cause)
    { super(cause); }
}
