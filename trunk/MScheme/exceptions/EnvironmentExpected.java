package MScheme.exceptions;

import MScheme.values.Value;


public class EnvironmentExpected
    extends TypeError
{
    public EnvironmentExpected(Value cause)
    { super(cause); }
}
