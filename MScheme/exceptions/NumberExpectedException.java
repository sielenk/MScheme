package MScheme.exceptions;

import MScheme.values.Value;


public class NumberExpectedException
    extends ExpectedException
{
    public NumberExpectedException(Value cause)
    { super(cause); }
}

