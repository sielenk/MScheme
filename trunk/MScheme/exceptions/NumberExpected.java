package MScheme.exceptions;

import MScheme.Value;


public class NumberExpected
    extends TypeError
{
    public NumberExpected(Value cause)
    { super(cause); }
}
