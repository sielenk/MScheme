package MScheme.exceptions;

import MScheme.values.Value;


public class FunctionExpected
    extends TypeError
{
    public FunctionExpected(Value cause)
    { super(cause); }
}

