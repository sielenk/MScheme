package MScheme.exceptions;

import MScheme.values.Value;


public class FunctionExpectedException
    extends ExpectedException
{
    public FunctionExpectedException(Value cause)
    { super(cause); }
}

