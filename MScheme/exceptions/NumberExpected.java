package MScheme.exceptions;

import MScheme.values.Value;


public class NumberExpected
    extends TypeError
{
    public NumberExpected(Value cause)
    { super(cause); }
}
