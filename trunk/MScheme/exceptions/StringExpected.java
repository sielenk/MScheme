package MScheme.exceptions;

import MScheme.Value;


public class StringExpected
    extends TypeError
{
    public StringExpected(Value cause)
    { super(cause); }
}
