package MScheme.exceptions;

import MScheme.values.Value;


public class StringExpected
    extends TypeError
{
    public StringExpected(Value cause)
    { super(cause); }
}
