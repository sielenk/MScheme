package MScheme.exceptions;

import MScheme.Value;


public class FunctionExpected
    extends TypeError
{
    public FunctionExpected(Value cause)
    { super(cause); }
}

